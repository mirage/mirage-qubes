open Formats.Qrexec
open Utils
open Lwt.Infix

module QV = Msg_chan.Make(Framing)

type t = QV.t

let (>>!=) = Msg_chan.(>>!=)

let split chr s =
  try
    let i = String.index s chr in
    Some (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))
  with Not_found ->
    None

let or_fail = function
  | `Ok y -> return y
  | `Error (`Unknown msg) -> fail (Failure msg)
  | `Eof -> fail End_of_file

let disconnect = QV.disconnect

let vchan_base_port =
  match Vchan.Port.of_string "512" with
  | `Error msg -> failwith msg
  | `Ok port -> port

let max_data_chunk = 4096
(** Max size for data chunks. See MAX_DATA_CHUNK in qubes-linux-utils/qrexec-lib/qrexec.h *)

let rec send t ~ty data =
  let data, data' = Cstruct.split data (min max_data_chunk (Cstruct.len data)) in
  let hdr = Cstruct.create sizeof_msg_header in
  set_msg_header_ty hdr (int_of_type ty);
  set_msg_header_len hdr (Cstruct.len data |> Int32.of_int);
  if Cstruct.len data' = 0
  then QV.send t [hdr; data]
  else QV.send t [hdr; data] >>= function
    | `Eof -> return `Eof
    | `Ok () ->
      send t ~ty data'

let recv t =
  QV.recv t >>!= fun (hdr, data) ->
  let ty = get_msg_header_ty hdr |> type_of_int in
  return (`Ok (ty, data))

let send_hello t =
  let hello = Cstruct.create sizeof_peer_info in
  set_peer_info_version hello 2l;
  send t ~ty:`Hello hello >>= function
  | `Eof -> fail (error "End-of-file sending msg_hello")
  | `Ok () -> return ()

let recv_hello t =
  recv t >>= function
  | `Eof -> fail (error "End-of-file waiting for msg_hello")
  | `Ok (`Hello, resp) -> return (get_peer_info_version resp)
  | `Ok (ty, _) -> fail (error "Expected msg_hello, got %ld" (int_of_type ty))

let port_of_int i =
  match Int32.to_string i |> Vchan.Port.of_string with
  | `Ok p -> p
  | `Error msg -> failwith msg

