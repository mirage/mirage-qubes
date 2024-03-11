(* Copyright (C) 2015, Thomas Leonard
   See the README file for details. *)

open Lwt.Infix
open Formats.QubesDB

let (>>!=) x f =
  x >>= function
  | `Ok y -> f y
  | `Eof -> Lwt.fail_with "qubesdb-agent: end-of-file from QubesDB!"
  | `Error (`Unknown msg) -> Fmt.failwith "qubesdb-agent: %s" msg

let starts_with str prefix =
  let ls = String.length str in
  let lp = String.length prefix in
  if lp > ls then false else
    let rec loop i =
      if i = lp then true
      else if str.[i] <> prefix.[i] then false
      else loop (i + 1)
    in loop 0

module QV = Msg_chan.Make(Framing)

let src = Logs.Src.create "qubes.db" ~doc:"QubesDB agent"
module Log = (val Logs.src_log src : Logs.LOG)

module KeyMap = Map.Make(String)

type t = {
  vchan : QV.t;
  notify : unit Lwt_condition.t;
  commit : string Lwt_condition.t;
  mutable store : string KeyMap.t;
  mutable committed_store : string KeyMap.t KeyMap.t;
}

let update t bindings =
  t.store <- bindings;
  Lwt_condition.broadcast t.notify ()

let qubesdb_vchan_port =
  match Vchan.Port.of_string "111" with
  | Error (`Msg msg) -> failwith msg
  | Ok port -> port

let send t ?(path="") ?(data="") ty =
  let data = Bytes.of_string data in
  let hdr = make_msg_header ~ty ~path ~data_len:(Bytes.length data) in
  QV.send t [hdr; data]

let recv t =
  QV.recv t >>!= fun (hdr, data) ->
  let ty =
    let ty = get_msg_header_ty hdr in
    match int_to_qdb_msg ty with
    | None -> Fmt.failwith "Invalid message type %d" ty
    | Some ty -> ty in
  let path = Bytes.to_string (get_msg_header_path hdr) in
  let path = String.sub path 0 (String.index path '\x00') in
  Lwt.return (ty, path, Bytes.to_string data)

let values_for_key store key =
  KeyMap.filter (fun k _ -> starts_with k (key ^ "/")) store

let full_db_sync t =
  send t.vchan QDB_CMD_MULTIREAD >>!= fun () ->
  let rec loop () =
    recv t.vchan >>= function
    | QDB_RESP_MULTIREAD, "", _ -> Lwt.return `Done
    | QDB_RESP_MULTIREAD, path, data ->
        Log.debug (fun f -> f "%S = %S" path data);
        t.store <- t.store |> KeyMap.add path data;
        loop ()
    | ty, _, _ -> Fmt.failwith "Unexpected QubesDB message: %s" (qdb_msg_to_string ty) in
  loop () >>= fun `Done ->
  Lwt_condition.broadcast t.notify ();  (* (probably not needed) *)
  KeyMap.iter (fun k v ->
    if v = "" then
      t.committed_store <- KeyMap.add k (values_for_key t.store k) t.committed_store)
    t.store;
  Lwt.return_unit

let rm t path =
  let len = String.length path in
  if len > 0 && path.[len - 1] = '/' then (
    (* Delete everything with this prefix *)
    t.store
    |> KeyMap.filter (fun key _ ->
      if starts_with key path then (
        Log.debug (fun f -> f "(rm %S)" key);
        false
      ) else true
    )
    |> update t ;
    let path_without_slash = String.sub path 0 (len - 1) in
    t.committed_store <- KeyMap.remove path_without_slash t.committed_store;
  ) else (
    if not (KeyMap.mem path t.store) then
      Log.err (fun f -> f "%S not found (fatal database de-synchronization)" path);
    t.store |> KeyMap.remove path |> update t
  )

let listen t =
  let rec loop () =
    let ack path =
      send t.vchan ~path QDB_RESP_OK >|= function
      | `Eof -> failwith "End-of-file sending OK"
      | `Ok () -> () in
    recv t.vchan >>= function
    | QDB_CMD_WRITE, path, value ->
        Log.info (fun f -> f "got update: %S = %S" path value);
        t.store |> KeyMap.add path value |> update t;
        ack path >>= fun () ->
        if value = "" then (
          t.committed_store <- KeyMap.add path (values_for_key t.store path) t.committed_store;
          Lwt_condition.broadcast t.commit path
        );
        loop ()
    | QDB_RESP_OK, path, _ ->
        Log.debug (fun f -> f "OK %S" path);
        loop ()
    | QDB_CMD_RM, path, _ ->
        Log.info (fun f -> f "got rm %S" path);
        rm t path;
        ack path >>= loop
    | QDB_RESP_ERROR, path, _ ->
        Log.err (fun f -> f "Error from peer (for %S)" path);
        loop ()
    | ty, _, _ ->
        Fmt.failwith "Unexpected QubesDB message: %s" (qdb_msg_to_string ty) in
  loop () >|= fun `Done -> ()

let connect ~domid () =
  Log.info (fun f -> f "connecting to server...");
  QV.client ~domid ~port:qubesdb_vchan_port () >>= fun vchan ->
  Log.info (fun f -> f "connected");
  let t = {vchan; store = KeyMap.empty; committed_store = KeyMap.empty; notify = Lwt_condition.create (); commit = Lwt_condition.create ()} in
  full_db_sync t >>= fun () ->
  Lwt.async (fun () -> listen t);
  Lwt.return t

let disconnect t : unit Lwt.t = QV.disconnect t.vchan

let read t key =
  try Some (KeyMap.find key t.store)
  with Not_found -> None

let write t key value =
  send t.vchan QDB_CMD_WRITE ~path:key ~data:value >>!= fun () ->
  Lwt.return_unit

let bindings t = t.store

let rec after t prev =
  let current = t.store in
  if KeyMap.equal (=) prev current then (
    Lwt_condition.wait t.notify >>= fun () ->
    after t prev
  ) else Lwt.return current

let find_committed_key t key =
  match KeyMap.find_opt key t.committed_store with
  | None -> KeyMap.empty
  | Some map -> map

let rec got_new_commit t key prev =
  let values = find_committed_key t key in
  if KeyMap.equal (=) prev values then (
    Lwt_condition.wait t.commit >>= fun key' ->
    if String.equal key key' then
      let values = find_committed_key t key in
      Lwt.return values
    else
      got_new_commit t key prev
  ) else Lwt.return values

(* Three sequences of events with commit can happen
1 - got_new_commit is called first, and waits for the commit
client: got_new_commit "/foo" empty -> empty = t.committed_store -> wait for condition
QubesDB: updates fuer /foo/bar
QubesDB: updates fuer /foo/bar2
QubesDB: commit fuer foo -> Lwt_conditioan.broadcast wakes up client
client: returns from got_new_commit

2 - got_new_commit while updates are in progress and not committed yet:
QubesDB: updates fuer /foo/bar
client: got_new_commit "/foo" empty => empty = t.committed_store -> wait for condition
QubesDB: updates fuer /foo/bar2
QubesDB: commit fuer foo -> t.committed_store <- t.store --> Lwt_condition.broadcast wakes up client
client: return from got_new_commit

3 - got_new_commit after commit:
QubesDB: updates fuer /foo/bar
QubesDB: updates fuer /foo/bar2
QubesDB: commit fuer foo -> t.committed_store <- t.store
client: got_new_commit "/foo" empty => empty <> t.committed_store
client: return from got_new_commit

4 - interleaving commits on qubesDB for different keys
QubesDB: updates for /foo/bar
QubesDB: updates for /bar/foo
QubesDB: commit for foo
client: got_new_commit "bar" empty
QubesDB: updates for /bar/foobar
QubesDB: commit for bar
client: return from got_new_commit
*)
