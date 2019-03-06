(* Copyright (C) 2015, Thomas Leonard
   See the README file for details. *)

open Lwt.Infix
open Formats.GUI
open Utils

module QV = Msg_chan.Make(Framing)

let src = Logs.Src.create "qubes.gui" ~doc:"Qubes GUId agent"
module Log = (val Logs.src_log src : Logs.LOG)

(* QUBES_GUID_PROTOCOL_VERSION_MAJOR << 16 | QUBES_GUID_PROTOCOL_VERSION_MINOR
   see ./qubes-gui-common/include/qubes-gui-protocol.h *)
let qubes_gui_protocol_version_linux = Int32.logor 0x1_0000_l 0x0001_l

let gui_agent_port =
  match Vchan.Port.of_string "6000" with
  | `Error msg -> failwith msg
  | `Ok port -> port

type t = QV.t

let connect ~domid () =
  Log.info (fun f -> f "waiting for client...");
  QV.server ~domid ~port:gui_agent_port () >>= fun gui ->
  (* qubesgui_init_connection *)
  let version = Cstruct.create sizeof_gui_protocol_version in
  set_gui_protocol_version_version version qubes_gui_protocol_version_linux;
  QV.send gui [version] >>= function
  | `Eof -> Lwt.fail (error "End-of-file sending protocol version")
  | `Ok () ->
  QV.recv_fixed gui sizeof_xconf >>= function
  | `Eof -> Lwt.fail (error "End-of-file getting X configuration")
  | `Ok conf ->
  let screen_w = get_xconf_w conf in
  let screen_h = get_xconf_h conf in
  let xdepth = get_xconf_depth conf in
  let xmem = get_xconf_mem conf in
  Log.info (fun f ->
      f "client connected (screen size: %ldx%ld depth: %ld mem: %ldx)"
        screen_w screen_h xdepth xmem);
  Lwt.return gui

let decode_KEYPRESS buf =
    let keypress : Formats.GUI.msg_keypress_t = {
      x = get_msg_keypress_x buf;
      y = get_msg_keypress_y buf;
      state = get_msg_keypress_state buf;
      keycode = get_msg_keypress_keycode buf;
    } in
    Log.warn (fun f ->
        f "Got a keypress ty: %ld x: %ld y: %ld state: %ld keycode: %ld"
             (get_msg_keypress_ty buf)
             keypress.Formats.GUI.x keypress.y keypress.state keypress.keycode)

let decode_FOCUS buf =
  let focus : Formats.GUI.msg_focus_t = {
    mode = get_msg_focus_mode buf;
    detail = get_msg_focus_detail buf;
  } in
  Log.warn (fun f ->
      f "Focus event: mode: %ld detail: %ld" focus.mode focus.detail)

let _decode_DESTROY buf =
  Log.warn (fun f -> f "DESTROY event: %s" (Cstruct.to_string buf))

let _decode_CLOSE buf =
  Log.warn (fun f -> f "CLOSE event: %s" (Cstruct.to_string buf))

let decode_CLIPBOARD_DATA buf =
  Log.warn (fun f ->
      f "Event: Received clipboard data from dom0: %S" (Cstruct.to_string buf))

let _decode_MSG_MOTION buf =
  match Formats.GUI.decode_msg_motion buf with
  | Some m ->
    Log.warn (fun f -> f "Motion event: x: %d y: %d state: %ld is_hint: %d"
                 m.x m.y m.state m.is_hint)
  | None ->
    Log.warn (fun f -> f "attempted to decode a motion event, but we were not successful")

let _decode_MSG_CROSSING buf =
  match decode_msg_crossing buf with
  | Some m ->
    Log.warn (fun f -> f "Event: CROSSING: type: %ld x: %ld y: %ld" m.ty m.x m.y)
  | None ->
    Log.warn (fun f -> f "attempted to decode a crossing event, but we were not successful")

let rec listen t =
  QV.recv t >>= function
  | `Eof -> failwith "End-of-file from GUId in dom0"
  | `Ok (msg_header , msg_buf) ->
  let msg_window = get_msg_header_window msg_header |> Int32.to_int in
  let msg_len    = get_msg_header_untrusted_len msg_header |> Int32.to_int in
  begin match int_to_msg_type (get_msg_header_ty msg_header) with

  (* handle fixed-length messages *)

  | Some ( MSG_KEYPRESS | MSG_BUTTON | MSG_MOTION | MSG_CROSSING | MSG_FOCUS
         | MSG_CREATE | MSG_DESTROY | MSG_CONFIGURE | MSG_SHMIMAGE
         | MSG_EXECUTE | MSG_WMNAME | MSG_KEYMAP_NOTIFY | MSG_WINDOW_HINTS
         | MSG_WINDOW_FLAGS | MSG_WMCLASS | MSG_CLIPBOARD_REQ
         | MSG_CLOSE as msg)
      when msg_len <> (match msg_type_size msg with Some x -> x | None -> -1) ->
      Log.warn (fun f -> f "BUG: expected_size [%d] <> msg_len [%d] for fixed-\
                            size msg! msg_header: %S Received raw buffer:: %S"
                         (match msg_type_size msg with Some x -> x | None -> -1)
                         msg_len
                         Cstruct.(to_string msg_header)
                         Cstruct.(to_string msg_buf))
  | Some MSG_KEYPRESS -> decode_KEYPRESS msg_buf
  | Some MSG_FOCUS -> decode_FOCUS msg_buf
  | Some MSG_MOTION -> ignore @@ decode_msg_motion msg_buf
  | Some MSG_CLIPBOARD_REQ ->
    Log.warn (fun f ->
        f "Event: dom0 requested our clipboard. debug: sizeof: %d"
          sizeof_msg_clipboard_req)
  | Some MSG_CROSSING -> ignore @@ decode_msg_crossing msg_buf
  | Some MSG_DESTROY ->
    Log.warn (fun f -> f "Event: DESTROY: %S" Cstruct.(to_string msg_buf))
  | Some MSG_CLOSE -> Log.warn (fun f -> f "Event: CLOSE window %d" msg_window)
  | Some MSG_BUTTON ->
    Log.warn (fun f -> f "Event: BUTTON: %S" Cstruct.(to_string msg_buf))
  | Some MSG_CREATE ->
    Log.warn (fun f -> f "Event: CREATE: %S" Cstruct.(to_string msg_buf))
  | Some MSG_EXECUTE ->
    Log.warn (fun f -> f "Event: EXECUTE: %S" Cstruct.(to_string msg_buf))
  | Some MSG_WMNAME ->
    Log.warn (fun f -> f "Event: WMNAME: %S" Cstruct.(to_string msg_buf))
  | Some MSG_KEYMAP_NOTIFY ->
    Log.warn (fun f -> f "Event: KEYMAP_NOTIFY: %S" Cstruct.(to_string msg_buf))
  | Some MSG_WINDOW_HINTS ->
    Log.warn (fun f -> f "Event: WINDOW_HINTS: %S" Cstruct.(to_string msg_buf))
  | Some MSG_WINDOW_FLAGS ->
    Log.warn (fun f -> f "Event: WINDOW_FLAGS: %S" Cstruct.(to_string msg_buf))
  | Some MSG_CONFIGURE ->
    Log.warn (fun f -> f "Event: CONFIGURE: %S" Cstruct.(to_string msg_buf))
  | Some MSG_SHMIMAGE
  | Some MSG_WMCLASS  ->
    Log.warn (fun f -> f "Event: Unhandled fixed-length: %S"
                 Cstruct.(to_string msg_buf))

  (* parse variable-length messages: *)

  | Some MSG_CLIPBOARD_DATA -> decode_CLIPBOARD_DATA msg_buf

  (* handle unimplemented/unexpected messages:*)

  | Some (MSG_MAP|MSG_UNMAP|MSG_MFNDUMP|MSG_DOCK) ->
    Log.warn (fun f ->
        f "UNHANDLED DATA of non-fixed length received. Data: %S"
          Cstruct.(to_string msg_buf))
  | None ->
    Log.warn (fun f -> f "Unexpected data with unknown type: [%S]%S"
                 (Cstruct.to_string msg_header) (Cstruct.to_string msg_buf))
  end
  ; listen t
