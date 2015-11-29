(* Copyright (C) 2015, Thomas Leonard
   See the README file for details. *)

open Utils
open Lwt.Infix
open Qubes_protocol.QubesDB

let (>>!=) x f =
  x >>= function
  | `Ok y -> f y
  | `Eof -> fail (Failure "qubesdb-agent: end-of-file from QubesDB!")
  | `Error (`Unknown msg) -> fail (error "qubesdb-agent: %s" msg)

module QV = Qvchan.Make(Qubes_protocol.QubesDB.Framing)

type t = {
  vchan : QV.t;
  store : (string, string) Hashtbl.t;
}

let qubesdb_vchan_port =
  match Vchan.Port.of_string "111" with
  | `Error msg -> failwith msg
  | `Ok port -> port

let empty = Cstruct.create 0

(* Copy str to the start of buffer and fill the rest with zeros *)
let set_fixed_string buffer str =
  let len = String.length str in
  Cstruct.blit_from_string str 0 buffer 0 len;
  Cstruct.memset (Cstruct.shift buffer len) 0

let send t ?(path="") ?(data=empty) ty =
  let data_len = Cstruct.len data in
  let msg = Cstruct.create (sizeof_msg_header + data_len) in
  set_msg_header_ty msg (qdb_msg_to_int ty);
  set_fixed_string (get_msg_header_path msg) path;
  set_msg_header_data_len msg (Int32.of_int data_len);
  Cstruct.blit data 0 msg sizeof_msg_header data_len;
  QV.send t [msg]

let recv t =
  QV.recv t >>!= fun (hdr, data) ->
  let ty =
    let ty = get_msg_header_ty hdr in
    match int_to_qdb_msg ty with
    | None -> raise (error "Invalid message type %d" ty)
    | Some ty -> ty in
  let path = Cstruct.to_string (get_msg_header_path hdr) in
  let path = String.sub path 0 (String.index path '\x00') in
  Lwt.return (ty, path, data)

let full_db_sync t =
  send t.vchan QDB_CMD_MULTIREAD >>!= fun () ->
  let rec loop () =
    recv t.vchan >>= function
    | QDB_RESP_MULTIREAD, "", _ -> return `Done
    | QDB_RESP_MULTIREAD, path, data ->
        let data = Cstruct.to_string data in
        Log.info "qubesDB: %S = %S" path data;
        Hashtbl.replace t.store path data;
        loop ()
    | ty, _, _ -> fail (error "Unexpected QubesDB message: %s" (qdb_msg_to_string ty)) in
  loop () >>= fun `Done ->
  return ()

let connect ~domid () =
  Log.info "qubesDB: connecting to server...";
  QV.client ~domid ~port:qubesdb_vchan_port () >>= fun vchan ->
  Log.info "qubesDB: connected";
  let t = {vchan; store = Hashtbl.create 20} in
  full_db_sync t >>= fun () ->
  Lwt.return t

let disconnect t = QV.disconnect t.vchan

let get t key =
  try Some (Hashtbl.find t.store key)
  with Not_found -> None
