(* Copyright (C) 2015, Thomas Leonard
   See the README file for details. *)

open Lwt

let unwrap_read = function
  | Error e -> `Error (Format.asprintf "%a" Vchan_xen.pp_error e)
  | Ok `Eof -> `Eof
  | Ok (`Data x) -> `Ok x

let (>>!=) x f =
  x >>= function
  | `Ok y -> f y
  | `Error msg -> fail (Failure msg)
  | `Eof -> return `Eof

module Make (F : Formats.FRAMING) = struct
  type t = {
    domid : int;
    vchan : Vchan_xen.flow;
    mutable buffer : String.t;
    read_lock : Lwt_mutex.t;
    write_lock : Lwt_mutex.t;
  }

  let rec read_exactly t size =
    let avail = String.length t.buffer in
    if avail >= size then (
      let retval = String.sub t.buffer 0 size in
      let new_buf = String.sub t.buffer size (avail-size) in
      t.buffer <- new_buf ;
      return (`Ok retval)
    ) else (
      Vchan_xen.read t.vchan >|= unwrap_read >>!= fun buf ->
      let new_buf = t.buffer ^ (Cstruct.to_string buf) in
      t.buffer <- new_buf ;
      read_exactly t size
    )

  let recv t =
    Lwt_mutex.with_lock t.read_lock (fun () ->
      read_exactly t F.header_size >>!= fun hdr ->
      read_exactly t (F.body_size_from_header hdr) >>!= fun body ->
      return (`Ok (hdr, body))
    )

  let recv_fixed t size =
    Lwt_mutex.with_lock t.read_lock (fun () ->
      read_exactly t size >>!= fun body ->
      return (`Ok body)
    )

  let recv_raw t : String.t S.or_eof Lwt.t =
    Lwt_mutex.with_lock t.read_lock @@ fun () ->
    if String.length t.buffer > 0 then (
      let data = t.buffer in
      t.buffer <- "";
      return (`Ok data)
    ) else (
      Vchan_xen.read t.vchan >|= unwrap_read >>!= fun result ->
      let data = Cstruct.to_string result in
      return (`Ok data)
    )

  let send t (buffers : String.t list) : unit S.or_eof Lwt.t =
    let cs = List.map Cstruct.of_string buffers in
    Lwt_mutex.with_lock t.write_lock (fun () ->
      Vchan_xen.writev t.vchan cs >>= function
      | Error `Closed -> return `Eof
      | Error e -> fail (Failure (Format.asprintf "%a" Vchan_xen.pp_write_error e))
      | Ok _ -> return @@ `Ok ()
    )

  let server ~domid ~port () =
    Vchan_xen.server ~domid ~port () >|= fun vchan -> {
      vchan;
      domid;
      buffer = "";
      read_lock = Lwt_mutex.create ();
      write_lock = Lwt_mutex.create ();
    }

  let client ~domid ~port () =
    Vchan_xen.client ~domid ~port () >|= fun vchan -> {
      vchan;
      domid;
      buffer = "";
      read_lock = Lwt_mutex.create ();
      write_lock = Lwt_mutex.create ();
    }

  let disconnect t : unit Lwt.t =
    Vchan_xen.close t.vchan
end
