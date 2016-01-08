(* Copyright (C) 2015, Thomas Leonard
   See the README file for details. *)

open Utils
open Lwt.Infix
open Formats.QubesDB

let (>>!=) x f =
  x >>= function
  | `Ok y -> f y
  | `Eof -> fail (Failure "qubesdb-agent: end-of-file from QubesDB!")
  | `Error (`Unknown msg) -> fail (error "qubesdb-agent: %s" msg)

module QV = Msg_chan.Make(Framing)

let src = Logs.Src.create "qubes.db" ~doc:"QubesDB agent"
module Log = (val Logs.src_log src : Logs.LOG)

module KeyMap = Map.Make(String)

type t = {
  vchan : QV.t;
  notify : unit Lwt_condition.t;
  mutable store : string KeyMap.t;
}

let update t bindings =
  t.store <- bindings;
  Lwt_condition.broadcast t.notify ()

let qubesdb_vchan_port =
  match Vchan.Port.of_string "111" with
  | `Error msg -> failwith msg
  | `Ok port -> port

let send t ?(path="") ?(data="") ty =
  let data = Cstruct.of_string data in
  let hdr = make_msg_header ~ty ~path ~data_len:(Cstruct.len data) in
  QV.send t [hdr; data]

let recv t =
  QV.recv t >>!= fun (hdr, data) ->
  let ty =
    let ty = get_msg_header_ty hdr in
    match int_to_qdb_msg ty with
    | None -> raise (error "Invalid message type %d" ty)
    | Some ty -> ty in
  let path = Cstruct.to_string (get_msg_header_path hdr) in
  let path = String.sub path 0 (String.index path '\x00') in
  Lwt.return (ty, path, Cstruct.to_string data)

let full_db_sync t =
  send t.vchan QDB_CMD_MULTIREAD >>!= fun () ->
  let rec loop () =
    recv t.vchan >>= function
    | QDB_RESP_MULTIREAD, "", _ -> return `Done
    | QDB_RESP_MULTIREAD, path, data ->
        Log.debug (fun f -> f "%S = %S" path data);
        t.store <- t.store |> KeyMap.add path data;
        loop ()
    | ty, _, _ -> fail (error "Unexpected QubesDB message: %s" (qdb_msg_to_string ty)) in
  loop () >>= fun `Done ->
  Lwt_condition.broadcast t.notify ();  (* (probably not needed) *)
  return ()

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
    |> update t
  ) else (
    if not (KeyMap.mem path t.store) then
      Log.err (fun f -> f "%S not found (fatal database de-synchronization)" path);
    t.store |> KeyMap.remove path |> update t
  )

let listen t =
  let rec loop () =
    let ack path =
      send t.vchan ~path QDB_RESP_OK >|= function
      | `Eof -> raise (error "End-of-file sending OK")
      | `Ok () -> () in
    recv t.vchan >>= function
    | QDB_CMD_WRITE, path, value ->
        Log.info (fun f -> f "got update: %S = %S" path value);
        t.store |> KeyMap.add path value |> update t;
        ack path >>= loop
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
        fail (error "Unexpected QubesDB message: %s" (qdb_msg_to_string ty)) in
  loop () >|= fun `Done -> ()

let connect ~domid () =
  Log.info (fun f -> f "connecting to server...");
  QV.client ~domid ~port:qubesdb_vchan_port () >>= fun vchan ->
  Log.info (fun f -> f "connected");
  let t = {vchan; store = KeyMap.empty; notify = Lwt_condition.create ()} in
  full_db_sync t >>= fun () ->
  Lwt.async (fun () -> listen t);
  Lwt.return t

let disconnect t = QV.disconnect t.vchan

let read t key =
  try Some (KeyMap.find key t.store)
  with Not_found -> None

let write t key value =
  send t.vchan QDB_CMD_WRITE ~path:key ~data:value >>!= fun () ->
  return ()

let bindings t = t.store

let rec after t prev =
  let current = t.store in
  if KeyMap.equal (=) prev current then (
    Lwt_condition.wait t.notify >>= fun () ->
    after t prev
  ) else return current
