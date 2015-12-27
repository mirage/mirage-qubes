(* Copyright (C) 2015, Thomas Leonard
   See the README file for details. *)

open Lwt

let (>>!=) x f =
  x >>= function
  | `Ok y -> f y
  | `Error (`Unknown msg) -> fail (Failure msg)
  | `Eof -> return `Eof

module Make (F : Formats.FRAMING) = struct
  type t = {
    domid : int;
    vchan : Vchan_xen.flow;
    mutable buffer : Cstruct.t;
    read_lock : Lwt_mutex.t;
    write_lock : Lwt_mutex.t;
  }

  let rec read_exactly t size =
    let avail = Cstruct.len t.buffer in
    if avail >= size then (
      let retval = Cstruct.sub t.buffer 0 size in
      t.buffer <- Cstruct.shift t.buffer size;
      return (`Ok retval)
    ) else (
      Vchan_xen.read t.vchan >>!= fun buf ->
      t.buffer <- Cstruct.append t.buffer buf;
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

  let recv_raw t =
    Lwt_mutex.with_lock t.read_lock @@ fun () ->
    if Cstruct.len t.buffer > 0 then (
      let data = t.buffer in
      t.buffer <- Cstruct.create 0;
      return (`Ok data)
    ) else (
      Vchan_xen.read t.vchan >>!= fun result ->
      return (`Ok result)
    )

  let send t buffers =
    Lwt_mutex.with_lock t.write_lock (fun () ->
      Vchan_xen.writev t.vchan buffers >>= function
      | `Error (`Unknown msg) -> fail (Failure msg)
      | `Ok () | `Eof as r -> return r
    )

  let server ~domid ~port () =
    Vchan_xen.server ~domid ~port () >|= fun vchan -> {
      vchan;
      domid;
      buffer = Cstruct.create 0;
      read_lock = Lwt_mutex.create ();
      write_lock = Lwt_mutex.create ();
    }

  let client ~domid ~port () =
    Vchan_xen.client ~domid ~port () >|= fun vchan -> {
      vchan;
      domid;
      buffer = Cstruct.create 0;
      read_lock = Lwt_mutex.create ();
      write_lock = Lwt_mutex.create ();
    }

  let disconnect t =
    Vchan_xen.close t.vchan
end
