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

type event =
  | UNIT of unit (* placeholder for unimplemented events *)
  | Keypress of msg_keypress_t
  | Focus of msg_focus_t
  | Motion of msg_motion_t
  | Clipboard_request
  | Clipboard_data of Cstruct.t
  | Configure of Formats.GUI.msg_configure_t
  | Window_crossing of msg_crossing_t
  | Window_destroy
  | Window_close
  | Button of msg_button_t

let pp_event fmt event =
  let pf() = Format.fprintf fmt in
  match event with
  | UNIT () -> pf() "UNIT"
  | Button _ -> pf() "Button"
  | Clipboard_request -> pf() "Clipboard_request"
  | Clipboard_data cs -> pf() "Clipboard_data: %S" (Cstruct.to_string cs)
  | Configure x -> pf() "Configure: @[x=%ld;@ y=%ld;@ width=%ld;@ height=%ld@]"
                     x.x x.y x.width x.height
  | Focus {mode;detail} -> pf() "Focus mode: %ld detail: %ld" mode detail
  | Keypress {x;y;state;keycode; ty = _ } ->
    pf() "Keypress x: %ld y: %ld state: %ld keycode: %ld" x y state keycode
  | Motion m -> pf() "Motion x: %d y: %d state: %ld is_hint: %d"
                      m.x m.y m.state m.is_hint
  | Window_close -> pf() "Window_close"
  | Window_crossing {ty;x;y ; state ; mode ; detail ; focus } ->
    pf() "Window_crossing type type: %ld x: %ld y: %ld \
          state: %ld mode: %ld detail: %ld focus: %ld "
      ty x y state mode detail focus
  | Window_destroy -> pf() "Window_destroy"

type window_id = Cstruct.uint32
type window = {no : window_id ; mvar : event Lwt_mvar.t ; qv : QV.t }
type t = { qv : QV.t ;
           mutable mvar : window list}

let decode_KEYPRESS buf =
    let keypress : Formats.GUI.msg_keypress_t = {
      x = get_msg_keypress_x buf;
      y = get_msg_keypress_y buf;
      state = get_msg_keypress_state buf;
      keycode = get_msg_keypress_keycode buf;
      ty = get_msg_keypress_ty buf;
    } in
    Keypress keypress

let decode_FOCUS buf =
  let focus : Formats.GUI.msg_focus_t = {
    mode = get_msg_focus_mode buf;
    detail = get_msg_focus_detail buf;
  } in
  Focus focus

let decode_MSG_CLOSE buf =
  Log.warn (fun f -> f "Event: CLOSE: %a" Cstruct.hexdump_pp buf) ;
  Window_close

let decode_CLIPBOARD_DATA buf =
  Log.warn (fun f -> f "Event: CLIPBOARD_DATA: %a" Cstruct.hexdump_pp buf);
  Clipboard_data buf

let int32_of_window (w : window) : int32 = w.no

let decode_MSG_MOTION buf =
  match Formats.GUI.decode_msg_motion buf with
  | Some m ->
    Log.warn (fun f -> f "Motion event: x: %d y: %d state: %ld is_hint: %d"
                 m.x m.y m.state m.is_hint);
    Motion m
  | None ->
    Log.warn (fun f -> f "attempted to decode a motion event, but we were not successful: %a" Cstruct.hexdump_pp buf);
    UNIT ()

let decode_CONFIGURE buf =
  match decode_msg_configure buf with
  | Some m -> Configure m
  | None ->
    Log.warn (fun f -> f "failed decoding CONFIGURE message from dom0: %a"
                 Cstruct.hexdump_pp buf) ;
    UNIT ()

let recv_event (window:window) =
  Lwt_mvar.take window.mvar

let debug_window w =
  let rec loop () = recv_event w >>= fun e ->
    Log.info (fun m -> m "debug_window [%ld]: %a" w.no pp_event e);
    loop ()
  in loop

let send t cs_lst = QV.send t.qv cs_lst

let set_title (window : window) title =
  QV.send window.qv
  [Formats.GUI.make_msg_wmname ~window:window.no ~wmname:title]

let create_window ?(parent=(0l:window_id)) ~x ~y ~title ~width ~height t
  : window S.or_eof Lwt.t =
  let w : window = { no = List.length t.mvar |> Int32.of_int ;
                     mvar = Lwt_mvar.create_empty () ;
                     qv = t.qv }
  in
  let window = w.no in
  Logs.warn (fun m -> m "Qubes.GUI: Creating new window id %ld" window);
  t.mvar <- w :: t.mvar ;
  let messages =
    let override_redirect = 0l in
    [Formats.GUI.make_msg_create ~width ~height ~x ~y
       ~override_redirect ~parent ~window ;
     Formats.GUI.make_msg_map_info ~override_redirect ~transient_for:0l ~window;
     Formats.GUI.make_msg_wmname ~window ~wmname:title ;
     Formats.GUI.make_msg_configure ~width ~height ~x ~y ~window ;
    ]
  in
  send t messages
  >>= function | `Ok () -> Lwt.return (`Ok w)
               | `Eof -> Lwt.return `Eof

let connect ~domid () =
  Log.info (fun f -> f "waiting for client...");
  QV.server ~domid ~port:gui_agent_port () >>= fun qv ->
  (* qubesgui_init_connection *)
  let version = Cstruct.create sizeof_gui_protocol_version in
  set_gui_protocol_version_version version qubes_gui_protocol_version_linux;
  QV.send qv [version] >>= function
  | `Eof -> Lwt.fail (error "End-of-file sending protocol version")
  | `Ok () ->
  QV.recv_fixed qv sizeof_xconf >>= function
  | `Eof -> Lwt.fail (error "End-of-file getting X configuration")
  | `Ok conf ->
  let screen_w = get_xconf_w conf in
  let screen_h = get_xconf_h conf in
  let xdepth = get_xconf_depth conf in
  let xmem = get_xconf_mem conf in
  Log.info (fun f ->
      f "client connected (screen size: %ldx%ld depth: %ld mem: %ldx)"
        screen_w screen_h xdepth xmem);
  let main_window = {no = 0l ; qv ; mvar = Lwt_mvar.create_empty ()} in
  Lwt.async (debug_window main_window) ;
  Lwt.return { qv ;
               mvar = [main_window] }

let rec listen t () =
  QV.recv t.qv >>= function
  | `Eof -> failwith "End-of-file from GUId in dom0"
  | `Ok (msg_header , msg_buf) ->
  let window = get_msg_header_window msg_header in
  let send_to_window event =
    match List.find (fun t -> t.no = window) t.mvar with
    | w -> Lwt_mvar.put w.mvar event
    | exception _ -> Log.warn (fun m -> m "No such window %ld" window);
                     Lwt.return ()
  in
  let msg_len    = get_msg_header_untrusted_len msg_header |> Int32.to_int in
  send_to_window
  begin match int_to_msg_type (get_msg_header_ty msg_header) with

  (* handle fixed-length messages *)

  | Some ( MSG_KEYPRESS | MSG_BUTTON | MSG_MOTION | MSG_CROSSING | MSG_FOCUS
         | MSG_CREATE | MSG_DESTROY | MSG_CONFIGURE | MSG_SHMIMAGE
         | MSG_EXECUTE | MSG_WMNAME | MSG_KEYMAP_NOTIFY | MSG_WINDOW_HINTS
         | MSG_WINDOW_FLAGS | MSG_WMCLASS | MSG_CLIPBOARD_REQ
         | MSG_CLOSE as msg)
    when (match msg_type_size msg with Some x -> x <> msg_len | None -> true) ->
    Log.warn (fun f -> f "BUG: expected_size [%d] <> msg_len [%d] for fixed-\
                          size msg! msg_header: %a@ Received raw buffer:: %a"
                 (match msg_type_size msg with Some x -> x | None -> -1)
                 msg_len
                 Cstruct.hexdump_pp msg_header
                 Cstruct.hexdump_pp msg_buf) ;
    UNIT()
  | Some MSG_MAP ->
    Log.warn (fun f -> f "Event: MAP: %a" Cstruct.hexdump_pp msg_buf) ;
    UNIT()
  | Some MSG_KEYPRESS -> decode_KEYPRESS msg_buf
  | Some MSG_FOCUS -> decode_FOCUS msg_buf
  | Some MSG_MOTION -> decode_MSG_MOTION msg_buf
  | Some MSG_CLIPBOARD_REQ ->
    Log.warn (fun f -> f "Event: dom0 requested our clipboard.") ;
    Clipboard_request
  | Some MSG_CROSSING -> begin match decode_msg_crossing msg_buf with
      | Some event -> Window_crossing event
      | None -> Log.warn (fun m -> m "Invalid MSG_CROSSING during decoding %a"
                             Cstruct.hexdump_pp msg_buf)
              ; UNIT ()
      end
  | Some MSG_CLOSE -> decode_MSG_CLOSE msg_buf
  | Some MSG_BUTTON -> begin match decode_msg_button msg_buf with
      | Some button_event -> Button button_event
      | None -> Log.warn (fun m -> m "Invalid MSG_BUTTON decoding %a"
                             Cstruct.hexdump_pp msg_buf)
        ; UNIT ()
      end
  | Some MSG_KEYMAP_NOTIFY ->
    (* Synchronize the keyboard state (key pressed/released) with dom0 *)
    Log.warn (fun f -> f "Event: KEYMAP_NOTIFY: %S"
      Cstruct.(to_string msg_buf)) ;
    UNIT()
  | Some MSG_WINDOW_FLAGS ->
    Log.warn (fun f -> f "Event: WINDOW_FLAGS: %S" Cstruct.(to_string msg_buf))
      ; UNIT ()
  | Some MSG_CONFIGURE ->
    Log.warn (fun f -> f "Event: CONFIGURE (should reply with this): %a"
                 Cstruct.hexdump_pp msg_buf) ;
    (* TODO here we are ACK'ing to Qubes that we accept the new dimensions -
            perhaps the user should have a say in that: *)
    decode_CONFIGURE msg_buf

  (* parse variable-length messages: *)

  | Some MSG_CLIPBOARD_DATA -> decode_CLIPBOARD_DATA msg_buf

  (* handle unimplemented/unexpected messages:*)

  | Some ( MSG_UNMAP | MSG_MFNDUMP | MSG_DOCK | MSG_WINDOW_HINTS
         | MSG_SHMIMAGE | MSG_WMCLASS | MSG_EXECUTE | MSG_CREATE
         | MSG_WMNAME | MSG_DESTROY ) ->
    (* Handle messages that are appvm->dom0 and thus dom0 is not supposed
       to send to the VM: *)
    Log.warn (fun f ->
        f "UNEXPECTED message received. Data: %a"
          Cstruct.hexdump_pp msg_buf); UNIT()
  | None ->
    Log.warn (fun f -> f "Unexpected data with unknown type: [%a] %aa"
                 Cstruct.hexdump_pp msg_header
                 Cstruct.hexdump_pp msg_buf) ;
    UNIT()
  end
  >>= fun () -> listen t ()
