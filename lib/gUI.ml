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
  | Focus {mode;detail} -> pf() "Focus mode: %ld detail: %ld" mode detail
  | Keypress {x;y;state;keycode} ->
    pf() "Keypress x: %ld y: %ld state: %ld keycode: %ld" x y state keycode
  | Motion m -> pf() "Motion x: %d y: %d state: %ld is_hint: %d"
                      m.x m.y m.state m.is_hint
  | Window_close -> pf() "Window_close"
  | Window_crossing {ty;x;y} ->
    pf() "Window_crossing type type: %ld x: %ld y: %ld" ty x y
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

let decode_MSG_DESTROY buf =
  Log.warn (fun f -> f "Event: DESTROY: %a" Cstruct.hexdump_pp buf) ;
  Window_destroy

let decode_MSG_CLOSE buf =
  Log.warn (fun f -> f "Event: CLOSE: %a" Cstruct.hexdump_pp buf) ;
  Window_close

let decode_CLIPBOARD_DATA buf = Clipboard_data buf

let decode_MSG_MOTION buf =
   let Some m = Formats.GUI.decode_msg_motion buf in
   Motion m

let decode_MSG_CROSSING buf =
  let Some m = decode_msg_crossing buf in
  Window_crossing m

let decode_MSG_BUTTON buf =
  Log.warn (fun f -> f "Event: BUTTON: %a" Cstruct.hexdump_pp buf) ;
  let Some m = decode_msg_button buf in Button m

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

let int32_of_window (w : window) : int32 = w.no

let create_window ?(parent=(0l:window_id)) ~width ~height t
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
    [Formats.GUI.make_msg_create ~width ~height ~x:0l ~y:0l
       ~override_redirect ~parent:0l ~window ;
     Formats.GUI.make_msg_map_info ~override_redirect ~transient_for:0l ~window;
     Formats.GUI.make_msg_wmname ~window ~wmname:"my window" ;
     Formats.GUI.make_msg_configure ~width ~height ~x:0l ~y:0l ~window ;
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
  let send_to_window =
    match List.find (fun t -> t.no = window) t.mvar with
    | w -> Lwt_mvar.put w.mvar
    | exception _ -> Log.warn (fun m -> m "No such window %ld" window);
                     fun _ -> Lwt.return ()
  in
  let msg_len    = get_msg_header_untrusted_len msg_header |> Int32.to_int in
  send_to_window @@
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
                         Cstruct.(to_string msg_buf)); UNIT ()
  | Some MSG_KEYPRESS -> decode_KEYPRESS msg_buf
  | Some MSG_FOCUS -> decode_FOCUS msg_buf
  | Some MSG_MOTION -> decode_MSG_MOTION msg_buf
  | Some MSG_CLIPBOARD_REQ ->
    Log.warn (fun f ->
        f "Event: dom0 requested our clipboard. debug: sizeof: %d"
          sizeof_msg_clipboard_req) ; Clipboard_request
  | Some MSG_CROSSING -> decode_MSG_CROSSING msg_buf
  | Some MSG_DESTROY -> decode_MSG_DESTROY msg_buf
  | Some MSG_CLOSE -> decode_MSG_CLOSE msg_buf
  | Some MSG_BUTTON -> decode_MSG_BUTTON msg_buf
  | Some MSG_CREATE ->
    Log.warn (fun f -> f "Event: CREATE: %S" Cstruct.(to_string msg_buf));
    UNIT ()
  | Some MSG_EXECUTE ->
    Log.warn (fun f -> f "Event: EXECUTE: %S" Cstruct.(to_string msg_buf));
    UNIT ()
  | Some MSG_WMNAME -> (* TODO VM -> dom0 only*)
    Log.err (fun f -> f "Event: WMNAME: %S" Cstruct.(to_string msg_buf)) ;
    UNIT ()
  | Some MSG_KEYMAP_NOTIFY ->
    Log.warn (fun f -> f "Event: KEYMAP_NOTIFY: %S" Cstruct.(to_string msg_buf))
    ;UNIT()
  | Some MSG_WINDOW_HINTS ->
    Log.warn (fun f -> f "Event: WINDOW_HINTS: %S" Cstruct.(to_string msg_buf))
    ;UNIT()
  | Some MSG_WINDOW_FLAGS ->
    Log.warn (fun f -> f "Event: WINDOW_FLAGS: %S" Cstruct.(to_string msg_buf))
    ; UNIT()
  | Some MSG_CONFIGURE ->
    Log.warn (fun f -> f "Event: CONFIGURE: %a" Cstruct.hexdump_pp msg_buf)
    ; UNIT()
  | Some MSG_SHMIMAGE
  | Some MSG_WMCLASS  ->
    Log.warn (fun f -> f "Event: Unhandled fixed-length: %S"
                 Cstruct.(to_string msg_buf)); UNIT()

  (* parse variable-length messages: *)

  | Some MSG_CLIPBOARD_DATA -> decode_CLIPBOARD_DATA msg_buf

  (* handle unimplemented/unexpected messages:*)

  | Some (MSG_MAP|MSG_UNMAP|MSG_MFNDUMP|MSG_DOCK) ->
    Log.warn (fun f ->
        f "UNHANDLED DATA of non-fixed length received. Data: %a"
          Cstruct.hexdump_pp msg_buf); UNIT()
  | None ->
    Log.warn (fun f -> f "Unexpected data with unknown type: [%a] %aa"
                 Cstruct.hexdump_pp msg_header
                 Cstruct.hexdump_pp msg_buf) ;
    UNIT()
  end
  >>= fun () -> listen t ()
