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

type t = {
  vchan : QV.t;
  store : (string, string) Hashtbl.t;
}

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
        Log.info "%S = %S" (fun f -> f path data);
        Hashtbl.replace t.store path data;
        loop ()
    | ty, _, _ -> fail (error "Unexpected QubesDB message: %s" (qdb_msg_to_string ty)) in
  loop () >>= fun `Done ->
  return ()

let rm t path =
  let len = String.length path in
  if len > 0 && path.[len - 1] = '/' then (
    (* Delete everything with this prefix *)
    let keys_to_remove =
      Hashtbl.fold (fun key _ acc ->
        if starts_with key path then (
          Log.info "(rm %S)" (fun f -> f key);
          key :: acc
        ) else acc
      ) t.store [] in
    keys_to_remove |> List.iter (Hashtbl.remove t.store)
  ) else (
    if not (Hashtbl.mem t.store path) then
      Log.err "%S not found (fatal database de-synchronization)" (fun f -> f path);
    Hashtbl.remove t.store path;
  )

let listen t =
  let rec loop () =
    let ack path =
      send t.vchan ~path QDB_RESP_OK >|= function
      | `Eof -> raise (error "End-of-file sending OK")
      | `Ok () -> () in
    recv t.vchan >>= function
    | QDB_CMD_WRITE, path, value ->
        Log.info "write %S = %S" (fun f -> f path value);
        Hashtbl.replace t.store path value;
        ack path >>= loop
    | QDB_RESP_OK, path, _ ->
        Log.info "OK %S" (fun f -> f path);
        loop ()
    | QDB_CMD_RM, path, _ ->
        Log.info "rm %S" (fun f -> f path);
        rm t path;
        ack path >>= loop
    | QDB_RESP_ERROR, path, _ ->
        Log.err "Error from peer (for %S)" (fun f -> f path);
        loop ()
    | ty, _, _ ->
        fail (error "Unexpected QubesDB message: %s" (qdb_msg_to_string ty)) in
  loop () >|= fun `Done -> ()

let connect ~domid () =
  Log.info "connecting to server..." Logs.unit;
  QV.client ~domid ~port:qubesdb_vchan_port () >>= fun vchan ->
  Log.info "connected" Logs.unit;
  let t = {vchan; store = Hashtbl.create 20} in
  full_db_sync t >>= fun () ->
  Lwt.async (fun () -> listen t);
  Lwt.return t

let disconnect t = QV.disconnect t.vchan

let read t key =
  try Some (Hashtbl.find t.store key)
  with Not_found -> None

let write t key value =
  send t.vchan QDB_CMD_WRITE ~path:key ~data:value >>!= fun () ->
  return ()
