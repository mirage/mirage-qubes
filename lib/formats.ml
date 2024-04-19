(** The Qubes wire protocol details. *)
(** for more details, see qubes-gui-common/include/qubes-gui-protocol.h *)

let of_int32_le i =
  let b = Bytes.create 4 in
  Bytes.set_int32_le b 0 i ;
  Bytes.unsafe_to_string b

(* String.get_* exixt since 4.13, this stub will be removed when the min
   Ocaml version will match *)
let get_int32_le s =
  Bytes.get_int32_le (Bytes.unsafe_of_string s)
let get_int32_be s =
  Bytes.get_int32_be (Bytes.unsafe_of_string s)
let get_uint8 s =
  Bytes.get_uint8 (Bytes.unsafe_of_string s)

module type FRAMING = sig
  val header_size : int
  val body_size_from_header : string -> int
end

module Qrexec = struct
      type msg_header = {
        ty : int32;
        len : int32;
      }
      let get_msg_header_ty h = get_int32_le h 0
      (* let set_msg_header_ty h v = Bytes.set_int32_le h 0 v *)
      let get_msg_header_len h = get_int32_le h 4
      (* let set_msg_heade.r_len h vString= Bytes.set_int32_le h 4 v *)
      let sizeof_msg_header = 8

      type peer_info = {
        version : int32;
      }
      let get_peer_info_version h = get_int32_le h 0
      (* let set_peer_info_version h v = Bytes.set_int32_le h 0 v *)
      let sizeof_peer_info = 4

      type exec_params = {
        connect_domain : int32;
        connect_port : int32;
        (* rest of message is command line *)
      }
      let get_exec_params_connect_domain h = get_int32_le h 0
      (* let set_exec_params_connect_domain h v = Bytes.set_int32_le h 0 v *)
      let get_exec_params_connect_port h = get_int32_le h 4
      (* let set_exec_params_connect_port h v = Bytes.set_int32_le h 4 v *)
      let sizeof_exec_params = 8

      type exit_status = {
        return_code : int32;
      }
      let get_exit_status_return_code h = get_int32_le h 0
      (* let set_exit_status_return_code h v = Bytes.set_int32_le h 0 v *)
      let sizeof_exit_status = 4

      type trigger_service_params = {
        service_name : string; (* [@len 64]; *)
        target_domain : string; (* [@len 32]; *)
        request_id : string; (* [@len 32] *)
      }
      let get_trigger_service_params_service_name h = String.sub h 0 64
      (* let set_trigger_service_params_service_name v ofs h = Bytes.blit_string v 0 h ofs 64 *)
      let get_trigger_service_params_target_domain h = String.sub h 64 32
      (* let set_trigger_service_params_target_domain v ofs h = Bytes.blit_string v 0 h (64+ofs) 32 *)
      let get_trigger_service_params_request_id h = String.sub h 96 32
      (* let set_trigger_service_params_request_id v ofs h = Bytes.blit_string v 0 h (96+ofs) 32 *)
      let sizeof_trigger_service_params = 64+32+32

      type trigger_service_params3 = {
        target_domain : string; (* [@len 64]; *)
        request_id : string; (* [@len 32] *)
        (* rest of message is service name *)
      }
      let get_trigger_service_params3_target_domain h = String.sub h 0 64
      (* let set_trigger_service_params3_target_domain v ofs h = Bytes.blit_string v 0 h ofs 64 *)
      let get_trigger_service_params3_request_id h = String.sub h 64 32
      (* let set_trigger_service_params3_request_id v ofs h = Bytes.blit_string v 0 h (64+ofs) 32 *)
      let sizeof_trigger_service_params3 = 64+32

  type msg_type =
    [ `Exec_cmdline
    | `Just_exec
    | `Service_connect
    | `Service_refused
    | `Trigger_service
    | `Connection_terminated
    | `Trigger_service3
    | `Hello
    | `Data_stdin
    | `Data_stdout
    | `Data_stderr
    | `Data_exit_code ]

  let type_of_int = function
    | 0x190l -> `Data_stdin
    | 0x191l -> `Data_stdout
    | 0x192l -> `Data_stderr
    | 0x193l -> `Data_exit_code
    | 0x200l -> `Exec_cmdline
    | 0x201l -> `Just_exec
    | 0x202l -> `Service_connect
    | 0x203l -> `Service_refused
    | 0x210l -> `Trigger_service
    | 0x211l -> `Connection_terminated
    | 0x212l -> `Trigger_service3
    | 0x300l -> `Hello
    | x -> `Unknown x

  let int_of_type = function
    | `Data_stdin -> 0x190l
    | `Data_stdout -> 0x191l
    | `Data_stderr -> 0x192l
    | `Data_exit_code -> 0x193l
    | `Exec_cmdline -> 0x200l
    | `Just_exec -> 0x201l
    | `Service_connect -> 0x202l
    | `Service_refused -> 0x203l
    | `Trigger_service -> 0x210l
    | `Connection_terminated -> 0x211l
    | `Trigger_service3 -> 0x212l
    | `Hello -> 0x300l
    | `Unknown x -> x

  let string_of_type = function
    | `Data_stdin -> "DATA_STDIN"
    | `Data_stdout -> "DATA_STDOUT"
    | `Data_stderr -> "DATA_STDERR"
    | `Data_exit_code -> "DATA_EXIT_CODE"
    | `Exec_cmdline -> "MSG_EXEC_CMDLINE"
    | `Just_exec -> "MSG_JUST_EXEC"
    | `Service_connect -> "MSG_SERVICE_CONNECT"
    | `Service_refused -> "MSG_SERVICE_REFUSED"
    | `Trigger_service -> "MSG_TRIGGER_SERVICE"
    | `Connection_terminated -> "MSG_CONNECTION_TERMINATED"
    | `Trigger_service3 -> "MSG_TRIGGER_SERVICE3"
    | `Hello -> "MSG_HELLO"
    | `Unknown x -> "Unknown message: " ^ (Int32.to_string x)

  type version =
    [ `V2
    | `V3 ]

  let version_of_int = function
    | 2l -> `V2
    | 3l -> `V3
    | x -> `Unknown_version x

  let int_of_version = function
    | `V2 -> 2l
    | `V3 -> 3l
    | `Unknown_version x -> x


  module Framing = struct
    let header_size = sizeof_msg_header
    let body_size_from_header h = get_msg_header_len h |> Int32.to_int
  end
end

module GUI = struct
  (** https://www.qubes-os.org/doc/gui/ *)
  (** see qubes-gui-common/include/qubes-gui-protocol.h *)

  let const_QUBES_MAIN_WINDOW = 1l

      type gui_protocol_version = {
        version : int32;
      }
      (* let get_gui_protocol_version_version h = Bytes.get_int32_le h 0 *)
      (* let set_gui_protocol_version_version h v = Bytes.set_int32_le h 0 v *)
      let sizeof_gui_protocol_version = 4


  (** struct msg_hdr *)
      type msg_header = {
        ty : int32; (** type *)
        window : int32;
        untrusted_len : int32;
      }
      let get_msg_header_ty h = get_int32_le h 0
      (* let set._msg_header_ty h v = Bytes.set_int32_le h 0 v *)
      let get_msg_header_window h = get_int32_le h 4
      (* let set_msg_header_window h v = Bytes.set_int32_le h 4 v *)
      let get_msg_header_untrusted_len h = get_int32_le h 8
      (* let set_msg_header_untrusted_len h v = Bytes.set_int32_le h 8 v *)
      let sizeof_msg_header = 12

  (** VM -> Dom0, Dom0 -> VM *)
      type msg_map_info = {
        override_redirect : int32;
        transient_for     : int32;
      }
      let get_msg_map_info_override_redirect h = get_int32_le h 0
      (* let set_msg_map_info_override_redirect h v = Bytes.set_int32_le h 0 v *)
      let get_msg_map_info_transient_for h = get_int32_le h 4
      (* let set_msg_map_info_transient_for h v = Bytes.set_int32_le h 4 v *)
      let sizeof_msg_map_info = 8

  (** Dom0 -> VM, dom0 wants us to reply with a MSG_CLIPBOARD_DATA *)
  let sizeof_msg_clipboard_req = 0

  (** Dom0 -> VM, VM -> Dom0: MSG_CLIPBOARD_DATA:
      a normal header, followed by a uint8 array of size len *)
    type msg_clipboard_data = {
      window_id : int32; (* [@big_endian]; *)
      len : int32;
      (* followed by a uint8 array of size len *)
    }
      let get_msg_clipboard_data_window_id h = get_int32_be h 0
      (* let set_msg_clipboard_data_window_id h v = Bytes.set_int32_be h 0 v *)
      let get_msg_clipboard_data_len h = get_int32_le h 4
      (* let set_msg_clipboard_data_len h v = Bytes.set_int32_le h 4 v *)
      let sizeof_msg_clipboard_data = 8

  (** VM -> Dom0 *)
      type msg_create = {
        x      : int32; (* position of window, seems to be converted *)
        y      : int32;
        width  : int32;
        height : int32; (* from qubes src: "size of image" *)
        parent : int32;
        override_redirect : int32;
      }
      let get_msg_create_x h = get_int32_le h 0
      (* let set_msg_create_x h v = Bytes.set_int32_le h 0 v *)
      let get_msg_create_y h = get_int32_le h 4
      (* let set_msg_create_y h v = Bytes.set_int32_le h 4 v *)
      let get_msg_create_width h = get_int32_le h 8
      (* let set_msg_create_width h v = Bytes.set_int32_le h 8 v *)
      let get_msg_create_height h = get_int32_le h 12
      (* let set_msg_create_height h v = Bytes.set_int32_le h 12 v *)
      let get_msg_create_parent h = get_int32_le h 16
      (* let set_msg_create_parent h v = Bytes.set_int32_le h 16 v *)
      let get_msg_create_override_redirect h = get_int32_le h 20
      (* let set_msg_create_override_redirect h v = Bytes.set_int32_le h 20 v *)
      let sizeof_msg_create = 24

  type msg_keypress_t =
    {
        ty    : int32; (* TODO make bool? XKeyEvent->type, see KeyPressMask/KeyReleaseMask *)
        x     : int32;
        y     : int32;
        state : int32; (* key mask *)
        keycode : int32;
    }

  (** Dom0 -> VM *)
  (* https://github.com/drinkcat/chroagh/commit/1d38c2e2422f97b6bf55580c9efc027ecf9f2721 *)
(*
      type msg_keypress = {
        ty    : int32;
        x     : int32;
        y     : int32;
        state : int32; (** 1:down, 0:up *)
        keycode : int32;
      }
*)
      let get_msg_keypress_ty h = get_int32_le h 0
      (* let set_msg_keypress_ty h v = Bytes.set_int32_le h 0 v *)
      let get_msg_keypress_x h = get_int32_le h 4
      (* let set_msg_keypress_x h v = Bytes.set_int32_le h 4 v *)
      let get_msg_keypress_y h = get_int32_le h 8
      (* let set_msg_keypress_y h v = Bytes.set_int32_le h 8 v *)
      let get_msg_keypress_state h = get_int32_le h 12
      (* let set_msg_keypress_state h v = Bytes.set_int32_le h 12 v *)
      let get_msg_keypress_keycode h = get_int32_le h 16
      (* let set_msg_keypress_keycode h v = Bytes.set_int32_le h 16 v *)
      let sizeof_msg_keypress = 20


  type msg_button_t = {
   ty : int32 ; (* TODO make bool? ButtonPress / ButtonRelease*)
    x : int32 ;
    y : int32 ;
    state : int32 ; (* button mask *)
    button: int32 ;
  }

(*
  (** Dom0 -> VM, TODO seems to be mouse buttons? *)
   type msg_button = {
     ty : int32;
     x : int32;
     y : int32;
     state : int32;
     button : int32; (* TODO *)
   }
*)
   let get_msg_button_ty h = get_int32_le h 0
   (* let set_msg_button_ty h v = Bytes.set_int32_le h 0 v *)
   let get_msg_button_x h = get_int32_le h 4
   (* let set_msg_button_x h v = Bytes.set_int32_le h 4 v *)
   let get_msg_button_y h = get_int32_le h 8
   (* let set_msg_button_y h v = Bytes.set_int32_le h 8 v *)
   let get_msg_button_state h = get_int32_le h 12
   (* let set_msg_button_state h v = Bytes.set_int32_le h 12 v *)
   let get_msg_button_button h = get_int32_le h 16
   (* let set_msg_button_button h v = Bytes.set_int32_le h 16 v *)
   let sizeof_msg_button = 20

  let decode_msg_button b: msg_button_t option =
    Some ({ ty = get_msg_button_ty b ;
            x = get_msg_button_x b ;
            y = get_msg_button_y b ;
            state = get_msg_button_state b ;
            button = get_msg_button_button b ;
      })

  (* dom0 -> VM, mouse / cursor movement *)
  type msg_motion_t = {
    x : int;
    y : int;
    state : int32;
    is_hint : int;
  }

      let get_msg_motion_x h = get_int32_le h 0
      let get_msg_motion_y h = get_int32_le h 4
      let get_msg_motion_state h = get_int32_le h 8
      let get_msg_motion_is_hint h = get_int32_le h 12
      let sizeof_msg_motion = 16

  let decode_msg_motion str : msg_motion_t option = (*TODO catch exceptions *)
  let i32 = fun f -> (f str |> Int32.to_int) in
  Some ({
      x = i32 get_msg_motion_x
   ;  y = i32 get_msg_motion_y
   ;  state  = get_msg_motion_state str
   ; is_hint = i32 get_msg_motion_is_hint
   } : msg_motion_t)

  (* Dom0 -> VM, TODO better types *)
  type msg_crossing_t = {
    ty : int32;
    x  : int32;
    y  : int32;
    state  : int32;
    mode   : int32;
    detail : int32;
    focus  : int32;
  }

(*
  (** Dom0 -> VM, seems to fire when the mouse is moved over a window border *)
   type msg_crossing = {
     ty : int32;
     x  : int32;
     y  : int32;
     state  : int32;
     mode   : int32;
     detail : int32;
     focus  : int32;
   }
*)
   let get_msg_crossing_ty h = get_int32_le h 0
   (* let set_msg_crossing_ty h v = Bytes.set_int32_le h 0 v *)
   let get_msg_crossing_x h = get_int32_le h 4
   (* let set_msg_crossing_x h v = Bytes.set_int32_le h 4 v *)
   let get_msg_crossing_y h = get_int32_le h 8
   (* let set_msg_crossing_y h v = Bytes.set_int32_le h 8 v *)
   let get_msg_crossing_state h = get_int32_le h 12
   (* let set_msg_crossing_state h v = Bytes.set_int32_le h 12 v *)
   let get_msg_crossing_mode h = get_int32_le h 16
   (* let set_msg_crossing_mode h v = Bytes.set_int32_le h 16 v *)
   let get_msg_crossing_detail h = get_int32_le h 20
   (* let set_msg_crossing_detail h v = Bytes.set_int32_le h 20 v *)
   let get_msg_crossing_focus h = get_int32_le h 24
   (* let set_msg_crossing_focus h v = Bytes.set_int32_le h 24 v *)
   let sizeof_msg_crossing = 28

  let decode_msg_crossing str  : msg_crossing_t option =
     (*TODO catch exceptions *)
    Some ({ ty = get_msg_crossing_ty str
          ;  x = get_msg_crossing_x  str
          ;  y = get_msg_crossing_y  str
          ;  state = get_msg_crossing_state  str
          ;   mode = get_msg_crossing_mode   str
          ; detail = get_msg_crossing_detail str
          ;  focus = get_msg_crossing_focus  str
          } : msg_crossing_t)

  (** VM -> Dom0, Dom0 -> VM, note that when you send this you must read the
                          "corrected" MSG_CONFIGURE you get back and use those
                          values instead of your own *)

  type msg_configure_t = {
    x: int32;
    y: int32;
    width: int32;
    height: int32;
    override_redirect: int32;
  }
(*      type msg_configure = {
        x      : int32;
        y      : int32;
        width  : int32;
        height : int32;
        override_redirect : int32;
      }
*)
      let get_msg_configure_x h = get_int32_le h 0
      (* let set_msg_configure_x h v = Bytes.set_int32_le h 0 v *)
      let get_msg_configure_y h = get_int32_le h 4
      (* let set_msg_configure_y h v = Bytes.set_int32_le h 4 v *)
      let get_msg_configure_width h = get_int32_le h 8
      (* let set_msg_configure_width h v = Bytes.set_int32_le h 8 v *)
      let get_msg_configure_height h = get_int32_le h 12
      (* let set_msg_configure_height h v = Bytes.set_int32_le h 12 v *)
      let get_msg_configure_override_redirect h = get_int32_le h 16
      (* let set_msg_configure_override_redirect h v = Bytes.set_int32_le h 16 v *)
      let sizeof_msg_configure = 20

  let decode_msg_configure b : msg_configure_t option =
    Some ({ x = get_msg_configure_x b ;
            y = get_msg_configure_y b ;
            width = get_msg_configure_width b ;
            height = get_msg_configure_height b ;
            override_redirect = get_msg_configure_override_redirect b ;
          } : msg_configure_t)

    (** VM -> Dom0 *)
      type msg_shmimage = {
        x : int32;
        y : int32;
        width : int32;
        height: int32;
      }
      let get_msg_shmimage_x h = get_int32_le h 0
      (* let set_msg_shmimage_x h v = Bytes.set_int32_le h 0 v *)
      let get_msg_shmimage_y h = get_int32_le h 4
      (* let set_msg_shmimage_y h v = Bytes.set_int32_le h 4 v *)
      let get_msg_shmimage_width h = get_int32_le h 8
      (* let set_msg_shmimage_width h v = Bytes.set_int32_le h 8 v *)
      let get_msg_shmimage_height h = get_int32_le h 12
      (* let set_msg_shmimage_height h v = Bytes.set_int32_le h 12 v *)
      let sizeof_msg_shmimage = 16


  type msg_focus_t = {
    mode : int32;
    detail: int32;
  }

  (** Dom0 -> VM *)
      type msg_focus = {
        ty     : int32;
        mode   : int32;
        detail : int32;
      }
      let get_msg_focus_ty h = get_int32_le h 0
      (* let set_msg_focus_ty h v = Bytes.set_int32_le h 0 v *)
      let get_msg_focus_mode h = get_int32_le h 4
      (* let set_msg_focus_mode h v = Bytes.set_int32_le h 4 v *)
      let get_msg_focus_detail h = get_int32_le h 8
      (* let set_msg_focus_detail h v = Bytes.set_int32_le h 8 v *)
      let sizeof_msg_focus = 12

  (* Dom0 -> VM *)
      type msg_execute = {
        cmd: string; (* uint8_t [@len 255]; *)
      }
      (* let get_msg_execute_cmd h = h *)
      (* let set_msg_execute_cmd h v = Bytes.blit v 0 h 0 255 *)
      let sizeof_msg_execute = 255

  (** Dom0 -> VM: Xorg conf *)
      type xconf = {
        w : int32; (** width *)
        h : int32; (** height *)
        depth : int32; (** bits per pixel *)
        mem : int32; (* TODO seemingly unused , could be: MemBase baseaddress
    This optional entry specifies the memory base address of a graphics board's
    linear frame buffer. This entry is not used by many drivers, and it should
    only be specified if the driver-specific documentation recommends it. *)
      }
      let get_xconf_w h = get_int32_le h 0
      (* let set_xconf_w h v = Bytes.set_int32_le h 0 v *)
      let get_xconf_h h = get_int32_le h 4
      (* let set_xconf_h h v = Bytes.set_int32_le h 4 v *)
      let get_xconf_depth h = get_int32_le h 8
      (* let set_xconf_depth h v = Bytes.set_int32_le h 8 v *)
      let get_xconf_mem h = get_int32_le h 12
      (* let set_xconf_mem h v = Bytes.set_int32_le h 12 v *)
      let sizeof_xconf = 16

  (* https://tronche.com/gui/x/icccm/sec-4.html#WM_TRANSIENT_FOR *)

  (** VM -> Dom0 *)
      type msg_wmname = {
        data : string (*uint8_t  [@len 128];*) (* title of the window *)
      }
      (* let get_msg_wmname_data h = h *)
      (* let set_msg_wmname_data h v = Bytes.blit v 0 h 0 128 *)
      let sizeof_msg_wmname = 128

  (** Dom0 -> VM *)
      type msg_keymap_notify = {
        (* this is a 256-bit bitmap of which keys should be enabled*)
        keys : string (*uint8_t [@len 32];*)
      }
      (* let get_msg_keymap_notify_keys h = h *)
      (* let set_msg_keymap_notify_keys h v = Bytes.blit v 0 h 0 32 *)
      let sizeof_msg_keymap_notify = 32

  (** VM -> Dom0 *)
  (* https://standards.freedesktop.org/wm-spec/latest/ *)
      type msg_window_hints = {
        flags : int32;
        min_width : int32;
        min_height: int32;
        max_width: int32;
        max_height: int32;
        width_inc: int32;
        height_inc: int32;
        base_width: int32;
        base_height: int32;
      }
      (* let get_msg_window_hints_flags h = String.get_int32_le h 0 *)
      (* let set_msg_window_hints_flags h v = Bytes.set_int32_le h 0 v *)
      (* let get_msg_window_hints_min_width h = String.get_int32_le h 4 *)
      (* let set_msg_window_hints_min_width h v = Bytes.set_int32_le h 4 v *)
      (* let get_msg_window_hints_min_height h = String.get_int32_le h 8 *)
      (* let set_msg_window_hints_min_height h v = Bytes.set_int32_le h 8 v *)
      (* let get_msg_window_hints_max_width h = String.get_int32_le h 12 *)
      (* let set_msg_window_hints_max_width h v = Bytes.set_int32_le h 12 v *)
      (* let get_msg_window_hints_max_height h = String.get_int32_le h 16 *)
      (* let set_msg_window_hints_max_height h v = Bytes.set_int32_le h 16 v *)
      (* let get_msg_window_hints_width_inc h = String.get_int32_le h 20 *)
      (* let set_msg_window_hints_width_inc h v = Bytes.set_int32_le h 20 v *)
      (* let get_msg_window_hints_height_inc h = String.get_int32_le h 24 *)
      (* let set_msg_window_hints_height_inc h v = Bytes.set_int32_le h 24 v *)
      (* let get_msg_window_hints_base_width h = String.get_int32_le h 28 *)
      (* let set_msg_window_hints_base_width h v = Bytes.set_int32_le h 28 v *)
      (* let get_msg_window_hints_base_height h = String.get_int32_le h 32 *)
      (* let set_msg_window_hints_base_height h v = Bytes.set_int32_le h 32 v *)
      let sizeof_msg_window_hints = 36

  (** VM -> Dom0, Dom0 -> VM *)
      type msg_window_flags = {
        (* &1= FULLSCREEN, &2= DEMANDS_ATTENTION, &4=MINIMIZE *)
        flags_set   : int32;
        flags_unset : int32;
      }
      (* let get_msg_window_flags_flags_set h = Bytes.get_int32_le h 0 *)
      (* let set_msg_window_flags_flags_set h v = Bytes.set_int32_le h 0 v *)
      (* let get_msg_window_flags_flags_unset h = Bytes.get_int32_le h 4 *)
      (* let set_msg_window_flags_flags_unset h v = Bytes.set_int32_le h 4 v *)
      let sizeof_msg_window_flags = 8

  (** VM -> Dom0 *)
      type shm_cmd = {
        shmid     : int32;
        width     : int32;
        height    : int32;
        bpp       : int32; (* bpp = bits per pixel *)
        off       : int32;
        num_mfn   : int32; (* number of pixels *)
        domid     : int32;
        (* followed by a variable length buffer of pixels:*)
        (* uint32_t mfns[0]; *)
      }
      (* let get_shm_cmd_shmid h = Bytes.get_int32_le h 0 *)
      (* let set_shm_cmd_shmid h v = Bytes.set_int32_le h 0 v *)
      (* let get_shm_cmd_width h = Bytes.get_int32_le h 4 *)
      (* let set_shm_cmd_width h v = Bytes.set_int32_le h 4 v *)
      (* let get_shm_cmd_height h = Bytes.get_int32_le h 8 *)
      (* let set_shm_cmd_height h v = Bytes.set_int32_le h 8 v *)
      (* let get_shm_cmd_bpp h = Bytes.get_int32_le h 12 *)
      (* let set_shm_cmd_bpp h v = Bytes.set_int32_le h 12 v *)
      (* let get_shm_cmd_off h = Bytes.get_int32_le h 16 *)
      (* let set_shm_cmd_off h v = Bytes.set_int32_le h 16 v *)
      (* let get_shm_cmd_num_mfn h = Bytes.get_int32_le h 20 *)
      (* let set_shm_cmd_num_mfn h v = Bytes.set_int32_le h 20 v *)
      (* let get_shm_cmd_domid h = Bytes.get_int32_le h 24 *)
      (* let set_shm_cmd_domid h v = Bytes.set_int32_le h 24 v *)
      let sizeof_shm_cmd = 28


  (** VM -> Dom0 *)
      type msg_wmclass = {
        res_class : string ; (* uint8_t [@len 64]; *)
        res_name : string ;(* uint8_t [@len 64]; *)
      }
      (* let get_msg_wmclass_res_class h = Bytes.sub h 0 64 *)
      (* let set_msg_wmclass_res_class h v = Bytes.blit v 0 h 0 64 *)
      (* let get_msg_wmclass_res_name h = Bytes.sub h 64 64 *)
      (* let set_msg_wmclass_res_name h v = Bytes.blit v 0 h 64 64 *)
      let sizeof_msg_wmclass = 128

    type msg_type =
    (*| MSG_MIN [@id 123l] (* 0x7b_l *) *)
    | MSG_KEYPRESS     (*[@id 124_l]*) (* 0x7c_l *)
    | MSG_BUTTON
    | MSG_MOTION
    | MSG_CROSSING
    | MSG_FOCUS
    (*| MSG_RESIZE - DEPRECATED; NOT IMPLEMENTED *)
    | MSG_CREATE        (*[@id 130_l]*) (* 0x82_l *)
    | MSG_DESTROY
    | MSG_MAP
    | MSG_UNMAP
    | MSG_CONFIGURE
    | MSG_MFNDUMP
    | MSG_SHMIMAGE
    | MSG_CLOSE
    | MSG_EXECUTE
    | MSG_CLIPBOARD_REQ
    | MSG_CLIPBOARD_DATA
    | MSG_WMNAME
    | MSG_KEYMAP_NOTIFY
    | MSG_DOCK
    | MSG_WINDOW_HINTS
    | MSG_WINDOW_FLAGS
    | MSG_WMCLASS
    (*| MSG_MAX [@id 147l]*)

  let msg_type_size = function
  | MSG_BUTTON -> Some sizeof_msg_button
  | MSG_CLIPBOARD_REQ -> Some sizeof_msg_clipboard_req
  | MSG_CLIPBOARD_DATA -> None
  | MSG_CLOSE -> Some 0 (* user clicked [X] or pressed Alt-F4 or similar *)
  | MSG_CONFIGURE -> Some sizeof_msg_configure
  | MSG_CREATE -> Some sizeof_msg_create
  | MSG_CROSSING -> Some sizeof_msg_crossing
  | MSG_DESTROY ->
    None (* this is the "prepare to shutdown your VM" message, no payload *)
  | MSG_DOCK -> None (* TODO *)
  | MSG_EXECUTE -> Some sizeof_msg_execute
  | MSG_FOCUS -> Some sizeof_msg_focus
  | MSG_KEYMAP_NOTIFY -> Some sizeof_msg_keymap_notify
  | MSG_KEYPRESS -> Some sizeof_msg_keypress
  | MSG_MAP -> None (* TODO *)
  | MSG_MFNDUMP -> None (* TODO *)
  | MSG_MOTION -> Some sizeof_msg_motion
  | MSG_SHMIMAGE -> Some sizeof_msg_shmimage
  | MSG_UNMAP -> None (* TODO *)
  | MSG_WINDOW_FLAGS -> Some sizeof_msg_window_flags
  | MSG_WINDOW_HINTS -> Some sizeof_msg_window_hints
  | MSG_WMCLASS -> Some sizeof_msg_wmclass
  | MSG_WMNAME -> Some sizeof_msg_wmname (* window title *)

  let msg_type_to_int = function
    (*| MSG_MIN -> 123l [@id 123l] (* 0x7b_l *) *)
    | MSG_KEYPRESS -> 124l     (*[@id 124_l]*) (* 0x7c_l *)
    | MSG_BUTTON -> 125l
    | MSG_MOTION -> 126l
    | MSG_CROSSING -> 127l
    | MSG_FOCUS -> 128l
    (*| MSG_RESIZE -> - DEPRECATED; NOT IMPLEMENTED *)
    | MSG_CREATE -> 130l        (*[@id 130_l]*) (* 0x82_l *)
    | MSG_DESTROY -> 131l
    | MSG_MAP -> 132l
    | MSG_UNMAP -> 133l
    | MSG_CONFIGURE -> 134l
    | MSG_MFNDUMP -> 135l
    | MSG_SHMIMAGE -> 136l
    | MSG_CLOSE -> 137l
    | MSG_EXECUTE -> 138l
    | MSG_CLIPBOARD_REQ -> 139l
    | MSG_CLIPBOARD_DATA -> 140l
    | MSG_WMNAME -> 141l
    | MSG_KEYMAP_NOTIFY -> 142l
    | MSG_DOCK -> 143l
    | MSG_WINDOW_HINTS -> 144l
    | MSG_WINDOW_FLAGS -> 145l
    | MSG_WMCLASS -> 146l
    (*| MSG_MAX [@id 147l]*)

  let int_to_msg_type = function
    (*| 123l -> Some MSG_MIN  [@id 123l] (* 0x7b_l *) *)
    | 124l -> Some MSG_KEYPRESS     (*[@id 124_l]*) (* 0x7c_l *)
    | 125l -> Some MSG_BUTTON
    | 126l -> Some MSG_MOTION
    | 127l -> Some MSG_CROSSING
    | 128l -> Some MSG_FOCUS
    (*| 124l -> Some MSG_RESIZE - DEPRECATED; NOT IMPLEMENTED *)
    | 130l -> Some MSG_CREATE         (*[@id 130_l]*) (* 0x82_l *)
    | 131l -> Some MSG_DESTROY
    | 132l -> Some MSG_MAP
    | 133l -> Some MSG_UNMAP
    | 134l -> Some MSG_CONFIGURE
    | 135l -> Some MSG_MFNDUMP
    | 136l -> Some MSG_SHMIMAGE
    | 137l -> Some MSG_CLOSE
    | 138l -> Some MSG_EXECUTE
    | 139l -> Some MSG_CLIPBOARD_REQ
    | 140l -> Some MSG_CLIPBOARD_DATA
    | 141l -> Some MSG_WMNAME
    | 142l -> Some MSG_KEYMAP_NOTIFY
    | 143l -> Some MSG_DOCK
    | 144l -> Some MSG_WINDOW_HINTS
    | 145l -> Some MSG_WINDOW_FLAGS
    | 146l -> Some MSG_WMCLASS
    (*| 147l -> Some MSG_MAX [@id 147l]*)
    | _ -> None

  (** "MFN: machine frame number - actual hw addresses"
http://ccrc.web.nthu.edu.tw/ezfiles/16/1016/img/598/v14n_xen.pdf
   *)
  (* type mfn : uint32_t;  big-endian 24-bit RGB pixel *)

  let make_with_header ~window ~ty ~body_len body =
    (** see qubes-gui-agent-linux/include/txrx.h:#define write_message *)
    String.concat "" [
      of_int32_le (msg_type_to_int ty) ;
      of_int32_le window ;
      of_int32_le body_len ;
      body
    ]

  let make_msg_mfndump ~window ~width ~height ~mfns =
    (* n.b. must be followed by a MSG_SHMIMAGE to actually repaint *)
    let num_mfn = List.length mfns in
    let offset  = 0x0l in
    (* TODO let n = (4 * width * height + offset
                     + (XC_PAGE_SIZE-1)) / XC_PAGE_SIZE; *)
    let cmds = mfns |> List.mapi (fun i -> fun _ ->
        of_int32_le (Int32.of_int (sizeof_shm_cmd + i*4))) in
    let body = String.concat "" @@ List.append [
        of_int32_le width ;
        of_int32_le height ;
        of_int32_le 24l ; (* bits per pixel *)
        of_int32_le offset ;
        of_int32_le @@ Int32.of_int (num_mfn) ;
    ] cmds in
    (* From https://www.qubes-os.org/doc/gui/
       >> "shmid" and "domid" parameters are just placeholders (to be filled
       >> by *qubes_guid* ), so that we can use the same structure when talking
       >> to shmoverride.so **)

    let body_len = Int32.of_int (sizeof_shm_cmd + num_mfn*4) in
    make_with_header ~window ~ty:MSG_MFNDUMP ~body_len body

  let make_msg_shmimage ~window ~x ~y ~width ~height =
    let body = String.concat "" [
        of_int32_le x ;
        of_int32_le y ;
        of_int32_le width ;
        of_int32_le height ;
    ] in
    let body_len = Int32.of_int sizeof_msg_shmimage in
    make_with_header ~window ~ty:MSG_SHMIMAGE ~body_len body

  let make_msg_create ~window ~width ~height ~x ~y ~override_redirect ~parent =
    let body = String.concat "" [
        of_int32_le width ;
        of_int32_le height ;
        of_int32_le x ;
        of_int32_le y ;
        of_int32_le override_redirect ;
        of_int32_le parent ;
    ] in
    let body_len = Int32.of_int sizeof_msg_create in
    make_with_header ~window ~ty:MSG_CREATE ~body_len body

  let make_msg_map_info ~window ~override_redirect ~transient_for =
    let body =
        of_int32_le override_redirect ^
        of_int32_le transient_for
    in
    let body_len = Int32.of_int sizeof_msg_map_info in
    make_with_header ~window ~ty:MSG_MAP ~body_len body

  let make_msg_wmname ~window ~wmname =
    let body =
        wmname ^
        String.make (sizeof_msg_wmname-String.(length wmname)) '\000' ; (* padding to sizeof_msg_wmname *)
    in
    let body_len = Int32.of_int sizeof_msg_wmname in
    make_with_header ~window ~ty:MSG_WMNAME ~body_len body

  let make_msg_window_hints ~window ~width ~height =
    let body = String.concat "" [
        of_int32_le @@ Int32.(16 lor 32 |> of_int) ;
       (*^--  PMinSize | PMaxSize *)
        of_int32_le width ;  (* min width *)
        of_int32_le height ; (* min height *)
        of_int32_le width ;  (* max width *)
        of_int32_le height ; (* max height *)
    ] in
    let body_len = Int32.of_int sizeof_msg_window_hints in
    make_with_header ~window ~ty:MSG_WINDOW_HINTS ~body_len body

  let make_msg_configure ~window ~x ~y ~width ~height =
    let body = String.concat "" [
        of_int32_le x ;
        of_int32_le y ; (* x and y are from qs->window_x and window_y*)
        of_int32_le width ;
        of_int32_le height ;
        of_int32_le 0l ; (* override_redirect *)
    ] in
    let body_len = Int32.of_int sizeof_msg_window_hints in
    make_with_header ~window ~ty:MSG_CONFIGURE ~body_len body

  module Framing = struct
    let header_size = sizeof_msg_header
    let body_size_from_header _h =
      get_msg_header_untrusted_len _h |> Int32.to_int
  end
end

module QubesDB = struct
      type qdb_msg =
        | QDB_CMD_READ
        | QDB_CMD_WRITE
        | QDB_CMD_MULTIREAD
        | QDB_CMD_LIST
        | QDB_CMD_RM
        | QDB_CMD_WATCH
        | QDB_CMD_UNWATCH
        | QDB_RESP_OK
        | QDB_RESP_ERROR_NOENT
        | QDB_RESP_ERROR
        | QDB_RESP_READ
        | QDB_RESP_MULTIREAD
        | QDB_RESP_LIST
        | QDB_RESP_WATCH

      let qdb_msg_to_int = function
        | QDB_CMD_READ -> 0
        | QDB_CMD_WRITE -> 1
        | QDB_CMD_MULTIREAD -> 2
        | QDB_CMD_LIST -> 3
        | QDB_CMD_RM -> 4
        | QDB_CMD_WATCH -> 5
        | QDB_CMD_UNWATCH -> 6
        | QDB_RESP_OK -> 7
        | QDB_RESP_ERROR_NOENT -> 8
        | QDB_RESP_ERROR -> 9
        | QDB_RESP_READ -> 10
        | QDB_RESP_MULTIREAD -> 11
        | QDB_RESP_LIST -> 12
        | QDB_RESP_WATCH -> 13

      let int_to_qdb_msg = function
        | 0 -> Some QDB_CMD_READ
        | 1 -> Some QDB_CMD_WRITE
        | 2 -> Some QDB_CMD_MULTIREAD
        | 3 -> Some QDB_CMD_LIST
        | 4 -> Some QDB_CMD_RM
        | 5 -> Some QDB_CMD_WATCH
        | 6 -> Some QDB_CMD_UNWATCH
        | 7 -> Some QDB_RESP_OK
        | 8 -> Some QDB_RESP_ERROR_NOENT
        | 9 -> Some QDB_RESP_ERROR
        | 10 -> Some QDB_RESP_READ
        | 11 -> Some QDB_RESP_MULTIREAD
        | 12 -> Some QDB_RESP_LIST
        | 13 -> Some QDB_RESP_WATCH
        | _ -> None

      let qdb_msg_to_string = function
        | QDB_CMD_READ -> "QDB_CMD_READ"
        | QDB_CMD_WRITE -> "QDB_CMD_WRITE"
        | QDB_CMD_MULTIREAD -> "QDB_CMD_MULTIREAD"
        | QDB_CMD_LIST -> "QDB_CMD_LIST"
        | QDB_CMD_RM -> "QDB_CMD_RM"
        | QDB_CMD_WATCH -> "QDB_CMD_WATCH"
        | QDB_CMD_UNWATCH -> "QDB_CMD_UNWATCH"
        | QDB_RESP_OK -> "QDB_RESP_OK"
        | QDB_RESP_ERROR_NOENT -> "QDB_RESP_ERROR_NOENT"
        | QDB_RESP_ERROR -> "QDB_RESP_ERROR"
        | QDB_RESP_READ -> "QDB_RESP_READ"
        | QDB_RESP_MULTIREAD -> "QDB_RESP_MULTIREAD"
        | QDB_RESP_LIST -> "QDB_RESP_LIST"
        | QDB_RESP_WATCH -> "QDB_RESP_WATCH"


      type msg_header = {
        ty        : int;
        path      : string; (* [@len 64]; *)
        padding   : string; (* [@len 3]; *)
        data_len  : int32;
        (* rest of message is data *)
      }
      let get_msg_header_ty h = get_uint8 h 0
      (* let set_msg_header_ty h v = Bytes.set_uint8 h 0 v *)
      let get_msg_header_path h = String.sub h 1 64
      (* let set_msg_header_path h v = Bytes.blit_string v 0 h 1 (min (String.length v) 64) *)
      let get_msg_header_data_len h = get_int32_le h 68
      (* let set_msg_header_data_len h v = Bytes.set_int32_le h 68 v *)
      let sizeof_msg_header = 72


  let make_msg_header ~ty ~path ~data_len =
    assert(String.length path <= 64);
    String.concat "" [
        String.make 1 (Char.chr (qdb_msg_to_int ty)) ; (* int8 *)
        path ;
        String.make (3+64-String.length path) '\000' ; (* padding=3 and max size of path=64 *)
        of_int32_le (Int32.of_int data_len) ;
    ]

  module Framing = struct
    let header_size = sizeof_msg_header
    let body_size_from_header h = get_msg_header_data_len h |> Int32.to_int
  end
end

module Rpc_filecopy = struct
  (* see qubes-linux-utils/qrexec-lib/libqubes-rpc-filecopy.h
   * and qubes-core-agent-windows/src/qrexec-services/common/filecopy.h*)
      type file_header = {
        namelen    : int32;
        mode       : int32;
        filelen    : int64;
        atime      : int32;
        atime_nsec : int32;
        mtime      : int32;
        mtime_nsec : int32;
      }
      (* followed by filename[namelen] and data[filelen] *)
      (* let get_file_header_namelen h = Bytes.get_int32_le h 0 *)
      (* let set_file_header_namelen h v = Bytes.set_int32_le h 0 v *)
      (* let get_file_header_mode h = Bytes.get_int32_le h 4 *)
      (* let set_file_header_mode h v = Bytes.set_int32_le h 4 v *)
      (* let get_file_header_filelen h = Bytes.get_int64_le h 8 *)
      (* let set_file_header_filelen h v = Bytes.set_int64_le h 8 v *)
      (* let get_file_header_atime h = Bytes.get_int32_le h 16 *)
      (* let set_file_header_atime h v = Bytes.set_int32_le h 16 v *)
      (* let get_file_header_atime_nsec h = Bytes.get_int32_le h 20 *)
      (* let set_file_header_atime_nsec h v = Bytes.set_int32_le h 20 v *)
      (* let get_file_header_mtime h = Bytes.get_int32_le h 24 *)
      (* let set_file_header_mtime h v = Bytes.set_int32_le h 24 v *)
      (* let get_file_header_mtime_nsec h = Bytes.get_int32_le h 28 *)
      (* let set_file_header_mtime_nsec h v = Bytes.set_int32_le h 28 v *)
      let sizeof_file_header = 32

      type result_header = {
        error_code : int32;
        _pad       : int32;
        crc32      : int64;
      }
      (* let get_result_header_error_code h = Bytes.get_int32_le h 0 *)
      (* let set_result_header_error_code h v = Bytes.set_int32_le h 0 v *)
      (* let get_result_header__pad h = Bytes.get_int32_le h 4 *)
      (* let set_result_header__pad h v = Bytes.set_int32_le h 4 v *)
      (* let get_result_header_crc32 h = Bytes.get_int64_le h 8 *)
      (* let set_result_header_crc32 h v = Bytes.set_int64_le h 8 v *)
      let sizeof_result_header = 16

      type result_header_ext = {
        last_namelen : int32;
        (* TODO char last_name[0]; variable length[last_namelen] *)
      }
      (* let get_result_header_ext_last_namelen h = Bytes.get_int32_le h 0 *)
      (* let set_result_header_ext_last_namelen h v = Bytes.set_int32_le h 0 v *)
      let sizeof_result_header_ext = 4

(*
  let make_result_header_ext last_filename =
    let namelen = Bytes.length last_filename in
    of_int32_le @@ (Int32.of_int namelen) ^ last_filename
*)

end
