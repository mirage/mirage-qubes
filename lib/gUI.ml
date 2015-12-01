(* Copyright (C) 2015, Thomas Leonard
   See the README file for details. *)

open Lwt.Infix
open Formats.GUI
open Utils

module QV = Msg_chan.Make(Framing)

let qubes_gui_protocol_version_linux = 0x10000l

let gui_agent_port =
  match Vchan.Port.of_string "6000" with
  | `Error msg -> failwith msg
  | `Ok port -> port

module Make(Log : S.LOG) = struct
  type t = QV.t

  let connect ~domid () =
    Log.info "waiting for client...";
    QV.server ~domid ~port:gui_agent_port () >>= fun gui ->
    let version = Cstruct.create sizeof_gui_protocol_version in
    set_gui_protocol_version_version version qubes_gui_protocol_version_linux;
    QV.send gui [version] >>= function
    | `Eof -> Lwt.fail (error "End-of-file sending protocol version")
    | `Ok () ->
    QV.recv_fixed gui sizeof_xconf >>= function
    | `Eof -> Lwt.fail (error "End-of-file getting X configuration")
    | `Ok conf ->
    let w = get_xconf_w conf in
    let h = get_xconf_h conf in
    Log.info "client connected (screen size: %ldx%ld)" w h;
    Lwt.return gui
end
