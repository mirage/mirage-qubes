(** The Qubes wire protocol details. *)
(** for more details, see qubes-gui-common/include/qubes-gui-protocol.h *)

module type FRAMING = sig
  val header_size : int
  val body_size_from_header : Bytes.t -> int
end

module Qrexec = struct
      type msg_header = {
        ty : int32;
        len : int32;
      }
      let get_msg_header_ty h = Bytes.get_int32_le h 0
      let set_msg_header_ty h v = Bytes.set_int32_le h 0 v
      let get_msg_header_len h = Bytes.get_int32_le h 4
      let set_msg_header_len h v = Bytes.set_int32_le h 4 v
      let sizeof_msg_header = 8

      type peer_info = {
        version : int32;
      }
      let get_peer_info_version h = Bytes.get_int32_le h 0
      let set_peer_info_version h v = Bytes.set_int32_le h 0 v
      let sizeof_peer_info = 4

      type exec_params = {
        connect_domain : int32;
        connect_port : int32;
        (* rest of message is command line *)
      }
      let get_exec_params_connect_domain h = Bytes.get_int32_le h 0
      let set_exec_params_connect_domain h v = Bytes.set_int32_le h 0 v
      let get_exec_params_connect_port h = Bytes.get_int32_le h 4
      let set_exec_params_connect_port h v = Bytes.set_int32_le h 4 v
      let sizeof_exec_params = 8

      type exit_status = {
        return_code : int32;
      }
      let get_exit_status_return_code h = Bytes.get_int32_le h 0
      let set_exit_status_return_code h v = Bytes.set_int32_le h 0 v
      let sizeof_exit_status = 4

      type trigger_service_params = {
        service_name : Bytes.t; (* [@len 64]; *)
        target_domain : Bytes.t; (* [@len 32]; *)
        request_id : Bytes.t; (* [@len 32] *)
      }
      let get_trigger_service_params_service_name h = Bytes.sub h 0 64
      let set_trigger_service_params_service_name v ofs h = Bytes.blit_string v 0 h ofs 64
      let get_trigger_service_params_target_domain h = Bytes.sub h 64 32
      let set_trigger_service_params_target_domain v ofs h = Bytes.blit_string v 0 h ofs 32
      let get_trigger_service_params_request_id h = Bytes.sub h 96 32
      let set_trigger_service_params_request_id v ofs h = Bytes.blit_string v 0 h ofs 32
      let sizeof_trigger_service_params = 64+32+32

      type trigger_service_params3 = {
        target_domain : Bytes.t; (* [@len 64]; *)
        request_id : Bytes.t; (* [@len 32] *)
        (* rest of message is service name *)
      }
      let get_trigger_service_params3_target_domain h = Bytes.sub h 0 64
      let set_trigger_service_params3_target_domain v ofs h = Bytes.blit_string v 0 h ofs 64
      let get_trigger_service_params3_request_id h = Bytes.sub h 64 32
      let set_trigger_service_params3_request_id v ofs h = Bytes.blit_string v 0 h ofs 32
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

  [%%cstruct
      type gui_protocol_version = {
        version : uint32_t;
      } [@@little_endian]
  ]

  (** struct msg_hdr *)
      type msg_header = {
        ty : int32; (** type *)
        window : int32;
        untrusted_len : int32;
      }
      let get_msg_header_ty h = Bytes.get_int32_le h 0
      let set_msg_header_ty h v = Bytes.set_int32_le h 0 v
      let get_msg_header_window h = Bytes.get_int32_le h 4
      let set_msg_header_window h v = Bytes.set_int32_le h 4 v
      let get_msg_header_untrusted_len h = Bytes.get_int32_le h 8
      let set_msg_header_untrusted_len h v = Bytes.set_int32_le h 8 v
      let sizeof_msg_header = 12

  (** VM -> Dom0, Dom0 -> VM *)
      type msg_map_info = {
        override_redirect : int32;
        transient_for     : int32;
      }
      let get_msg_map_info_override_redirect h = Bytes.get_int32_le h 0
      let set_msg_map_info_override_redirect h v = Bytes.set_int32_le h 0 v
      let get_msg_map_info_transient_for h = Bytes.get_int32_le h 4
      let set_msg_map_info_transient_for h v = Bytes.set_int32_le h 4 v
      let sizeof_msg_map_info = 8

  (** Dom0 -> VM, dom0 wants us to reply with a MSG_CLIPBOARD_DATA *)
  let sizeof_msg_clipboard_req = 0

  (** Dom0 -> VM, VM -> Dom0: MSG_CLIPBOARD_DATA:
      a normal header, followed by a uint8 array of size len *)
  [%%cstruct
    type msg_clipboard_data = {
      window_id : uint32_t [@big_endian];
      len : uint32_t;
      (* followed by a uint8 array of size len *)
    } [@@little_endian]
  ]

  (** VM -> Dom0 *)
      type msg_create = {
        x      : int32; (* position of window, seems to be converted *)
        y      : int32;
        width  : int32;
        height : int32; (* from qubes src: "size of image" *)
        parent : int32;
        override_redirect : int32;
      }
      let get_msg_create_x h = Bytes.get_int32_le h 0
      let set_msg_create_x h v = Bytes.set_int32_le h 0 v
      let get_msg_create_y h = Bytes.get_int32_le h 4
      let set_msg_create_y h v = Bytes.set_int32_le h 4 v
      let get_msg_create_width h = Bytes.get_int32_le h 8
      let set_msg_create_width h v = Bytes.set_int32_le h 8 v
      let get_msg_create_height h = Bytes.get_int32_le h 12
      let set_msg_create_height h v = Bytes.set_int32_le h 12 v
      let get_msg_create_parent h = Bytes.get_int32_le h 16
      let set_msg_create_parent h v = Bytes.set_int32_le h 16 v
      let get_msg_create_override_redirect h = Bytes.get_int32_le h 20
      let set_msg_create_override_redirect h v = Bytes.set_int32_le h 20 v
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
  [%%cstruct
      type msg_keypress = {
        ty    : uint32_t;
        x     : uint32_t;
        y     : uint32_t;
        state : uint32_t; (** 1:down, 0:up *)
        keycode : uint32_t;
      } [@@little_endian]
  ]

  type msg_button_t = {
   ty : int32 ; (* TODO make bool? ButtonPress / ButtonRelease*)
    x : int32 ;
    y : int32 ;
    state : int32 ; (* button mask *)
    button: int32 ;
  }

  (** Dom0 -> VM, TODO seems to be mouse buttons? *)
  [%%cstruct
   type msg_button = {
     ty : uint32_t;
     x : uint32_t;
     y : uint32_t;
     state : uint32_t;
     button : uint32_t; (* TODO *)
   } [@@little_endian]
  ]

  let decode_msg_button cs : msg_button_t option =
    Some ({ ty = get_msg_button_ty cs ;
            x = get_msg_button_x cs ;
            y = get_msg_button_y cs ;
            state = get_msg_button_state cs ;
            button = get_msg_button_button cs ;
      })

  (* dom0 -> VM, mouse / cursor movement *)
  type msg_motion_t = {
    x : int;
    y : int;
    state : int32;
    is_hint : int;
  }

  (** Dom0 -> VM, mouse / cursor motion event *)
  [%%cstruct
      type msg_motion = {
        x       : uint32_t;
        y       : uint32_t;
        state   : uint32_t;
        is_hint : uint32_t;
      } [@@little_endian]
  ]

  let decode_msg_motion cs : msg_motion_t option = (*TODO catch exceptions *)
  let i32 = fun f -> (f cs |> Int32.to_int) in
  Some ({
      x = i32 get_msg_motion_x
   ;  y = i32 get_msg_motion_y
   ;  state  = get_msg_motion_state cs
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

  (** Dom0 -> VM, seems to fire when the mouse is moved over a window border *)
  [%%cstruct
   type msg_crossing = {
     ty : uint32_t;
     x  : uint32_t;
     y  : uint32_t;
     state  : uint32_t;
     mode   : uint32_t;
     detail : uint32_t;
     focus  : uint32_t;
   } [@@little_endian]
  ]

  let decode_msg_crossing cs  : msg_crossing_t option =
     (*TODO catch exceptions *)
    Some ({ ty = get_msg_crossing_ty cs
          ;  x = get_msg_crossing_x  cs
          ;  y = get_msg_crossing_y  cs
          ;  state = get_msg_crossing_state  cs
          ;   mode = get_msg_crossing_mode   cs
          ; detail = get_msg_crossing_detail cs
          ;  focus = get_msg_crossing_focus  cs
          } : msg_crossing_t)

  (** VM -> Dom0, Dom0 -> VM, note that when you send this you must read the
                          "corrected" MSG_CONFIGURE you get back and use those
                          values instead of your own *)
      type msg_configure = {
        x      : int32;
        y      : int32;
        width  : int32;
        height : int32;
        override_redirect : int32;
      }
      let get_msg_configure_x h = Bytes.get_int32_le h 0
      let set_msg_configure_x h v = Bytes.set_int32_le h 0 v
      let get_msg_configure_y h = Bytes.get_int32_le h 4
      let set_msg_configure_y h v = Bytes.set_int32_le h 4 v
      let get_msg_configure_width h = Bytes.get_int32_le h 8
      let set_msg_configure_width h v = Bytes.set_int32_le h 8 v
      let get_msg_configure_height h = Bytes.get_int32_le h 12
      let set_msg_configure_height h v = Bytes.set_int32_le h 12 v
      let get_msg_configure_override_redirect h = Bytes.get_int32_le h 16
      let set_msg_configure_override_redirect h v = Bytes.set_int32_le h 16 v
      let sizeof_msg_configure = 20

  type msg_configure_t = {
    x: int32;
    y: int32;
    width: int32;
    height: int32;
    override_redirect: int32;
  }

  let decode_msg_configure cs : msg_configure_t option =
    Some ({ x = get_msg_configure_x cs ;
            y = get_msg_configure_y cs ;
            width = get_msg_configure_width cs ;
            height = get_msg_configure_height cs ;
            override_redirect = get_msg_configure_override_redirect cs ;
          } : msg_configure_t)

    (** VM -> Dom0 *)
      type msg_shmimage = {
        x : int32;
        y : int32;
        width : int32;
        height: int32;
      }
      let get_msg_shmimage_x h = Bytes.get_int32_le h 0
      let set_msg_shmimage_x h v = Bytes.set_int32_le h 0 v
      let get_msg_shmimage_y h = Bytes.get_int32_le h 4
      let set_msg_shmimage_y h v = Bytes.set_int32_le h 4 v
      let get_msg_shmimage_width h = Bytes.get_int32_le h 8
      let set_msg_shmimage_width h v = Bytes.set_int32_le h 8 v
      let get_msg_shmimage_height h = Bytes.get_int32_le h 12
      let set_msg_shmimage_height h v = Bytes.set_int32_le h 12 v
      let sizeof_msg_shmimage = 16


  type msg_focus_t = {
    mode : Cstruct.uint32;
    detail: Cstruct.uint32;
  }

  (** Dom0 -> VM *)
  [%%cstruct
      type msg_focus = {
        ty     : uint32_t;
        mode   : uint32_t;
        detail : uint32_t;
      } [@@little_endian]
  ]

  (* Dom0 -> VM *)
  [%%cstruct
      type msg_execute = {
        cmd: uint8_t [@len 255];
      } [@@little_endian]
  ]

  (** Dom0 -> VM: Xorg conf *)
  [%%cstruct
      type xconf = {
        w : uint32_t; (** width *)
        h : uint32_t; (** height *)
        depth : uint32_t; (** bits per pixel *)
        mem : uint32_t; (* TODO seemingly unused , could be: MemBase baseaddress
    This optional entry specifies the memory base address of a graphics board's
    linear frame buffer. This entry is not used by many drivers, and it should
    only be specified if the driver-specific documentation recommends it. *)
      } [@@little_endian]
  ]

  (* https://tronche.com/gui/x/icccm/sec-4.html#WM_TRANSIENT_FOR *)

  (** VM -> Dom0 *)
  [%%cstruct
      type msg_wmname = {
        data : uint8_t  [@len 128]; (* title of the window *)
      } [@@little_endian]
  ]

  (** Dom0 -> VM *)
  [%%cstruct
   type msg_keymap_notify = {
     (* this is a 256-bit bitmap of which keys should be enabled*)
     keys : uint8_t [@len 32];
   } [@@little_endian]
  ]

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
      let get_msg_window_hints_flags h = Bytes.get_int32_le h 0
      let set_msg_window_hints_flags h v = Bytes.set_int32_le h 0 v
      let get_msg_window_hints_min_width h = Bytes.get_int32_le h 4
      let set_msg_window_hints_min_width h v = Bytes.set_int32_le h 4 v
      let get_msg_window_hints_min_height h = Bytes.get_int32_le h 8
      let set_msg_window_hints_min_height h v = Bytes.set_int32_le h 8 v
      let get_msg_window_hints_max_width h = Bytes.get_int32_le h 12
      let set_msg_window_hints_max_width h v = Bytes.set_int32_le h 12 v
      let get_msg_window_hints_max_height h = Bytes.get_int32_le h 16
      let set_msg_window_hints_max_height h v = Bytes.set_int32_le h 16 v
      let get_msg_window_hints_width_inc h = Bytes.get_int32_le h 20
      let set_msg_window_hints_width_inc h v = Bytes.set_int32_le h 20 v
      let get_msg_window_hints_height_inc h = Bytes.get_int32_le h 24
      let set_msg_window_hints_height_inc h v = Bytes.set_int32_le h 24 v
      let get_msg_window_hints_base_width h = Bytes.get_int32_le h 28
      let set_msg_window_hints_base_width h v = Bytes.set_int32_le h 28 v
      let get_msg_window_hints_base_height h = Bytes.get_int32_le h 32
      let set_msg_window_hints_base_height h v = Bytes.set_int32_le h 32 v
      let sizeof_msg_window_hints = 36

  (** VM -> Dom0, Dom0 -> VM *)
  [%%cstruct
   type msg_window_flags = {
     (* &1= FULLSCREEN, &2= DEMANDS_ATTENTION, &4=MINIMIZE *)
     flags_set   : uint32_t;
     flags_unset : uint32_t;
   } [@@little_endian]
  ]

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
      let get_shm_cmd_shmid h = Bytes.get_int32_le h 0
      let set_shm_cmd_shmid h v = Bytes.set_int32_le h 0 v
      let get_shm_cmd_width h = Bytes.get_int32_le h 4
      let set_shm_cmd_width h v = Bytes.set_int32_le h 4 v
      let get_shm_cmd_height h = Bytes.get_int32_le h 8
      let set_shm_cmd_height h v = Bytes.set_int32_le h 8 v
      let get_shm_cmd_bpp h = Bytes.get_int32_le h 12
      let set_shm_cmd_bpp h v = Bytes.set_int32_le h 12 v
      let get_shm_cmd_off h = Bytes.get_int32_le h 16
      let set_shm_cmd_off h v = Bytes.set_int32_le h 16 v
      let get_shm_cmd_num_mfn h = Bytes.get_int32_le h 20
      let set_shm_cmd_num_mfn h v = Bytes.set_int32_le h 20 v
      let get_shm_cmd_domid h = Bytes.get_int32_le h 24
      let set_shm_cmd_domid h v = Bytes.set_int32_le h 24 v
      let sizeof_shm_cmd = 28
      

  (** VM -> Dom0 *)
  [%%cstruct
   type msg_wmclass = {
     res_class : uint8_t [@len 64];
     res_name : uint8_t [@len 64];
   } [@@little_endian]
  ]

  [%%cenum
    type msg_type =
    (*| MSG_MIN [@id 123l] (* 0x7b_l *) *)
    | MSG_KEYPRESS     [@id 124_l] (* 0x7c_l *)
    | MSG_BUTTON
    | MSG_MOTION
    | MSG_CROSSING
    | MSG_FOCUS
    (*| MSG_RESIZE - DEPRECATED; NOT IMPLEMENTED *)
    | MSG_CREATE        [@id 130_l] (* 0x82_l *)
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
    [@@uint32_t]
  ]

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

  (** "MFN: machine frame number - actual hw addresses"
http://ccrc.web.nthu.edu.tw/ezfiles/16/1016/img/598/v14n_xen.pdf
   *)
  (* type mfn : uint32_t;  big-endian 24-bit RGB pixel *)

  let make_with_header ~window ~ty body =
    (** see qubes-gui-agent-linux/include/txrx.h:#define write_message *)
    (** TODO consider using Cstruct.add_len *)
    let body_len = Bytes.length body in
    let msg = Bytes.create (sizeof_msg_header + body_len) in
    let()= set_msg_header_ty     msg (msg_type_to_int ty) in
    let()= set_msg_header_window msg window in
    let()= set_msg_header_untrusted_len msg Int32.(of_int body_len) in
    let() = Bytes.blit
        (* src, srcoff: *) body 0
        (* dst, dstoff: *) msg sizeof_msg_header
        (* length: *)      Bytes.(length body)
    in msg

  let make_msg_mfndump ~window ~width ~height ~mfns =
    (* n.b. must be followed by a MSG_SHMIMAGE to actually repaint *)
    let num_mfn = List.length mfns in
    let offset  = 0x0l in
    let body = Bytes.create (sizeof_shm_cmd + num_mfn*4) in
    set_shm_cmd_width   body width;
    set_shm_cmd_height  body height;
    set_shm_cmd_bpp     body 24l; (* bits per pixel *)
    set_shm_cmd_off     body offset;
    set_shm_cmd_num_mfn body Int32.(of_int num_mfn);
    (* From https://www.qubes-os.org/doc/gui/
       >> "shmid" and "domid" parameters are just placeholders (to be filled
       >> by *qubes_guid* ), so that we can use the same structure when talking
       >> to shmoverride.so **)

    (* TODO let n = (4 * width * height + offset
                     + (XC_PAGE_SIZE-1)) / XC_PAGE_SIZE; *)
    mfns |> List.iteri (fun i ->
        Bytes.set_int32_le body (sizeof_shm_cmd + i*4));
    make_with_header ~window ~ty:MSG_MFNDUMP body

  let make_msg_shmimage ~window ~x ~y ~width ~height =
    let body = Bytes.create (sizeof_msg_shmimage) in
    set_msg_shmimage_x body x;
    set_msg_shmimage_y body y;
    set_msg_shmimage_width body width;
    set_msg_shmimage_height body height;
    make_with_header ~window ~ty:MSG_SHMIMAGE body

  let make_msg_create ~window ~width ~height ~x ~y ~override_redirect ~parent =
    let body = Bytes.create sizeof_msg_create in
    set_msg_create_width             body width; (*  w *)
    set_msg_create_height            body height; (* h *)
    set_msg_create_x                 body x;
    set_msg_create_y                 body y;
    set_msg_create_override_redirect body override_redirect;
    set_msg_create_parent            body parent;
    make_with_header ~window ~ty:MSG_CREATE body

  let make_msg_map_info ~window ~override_redirect ~transient_for =
    let body = Bytes.create sizeof_msg_map_info in
    let()= set_msg_map_info_override_redirect body override_redirect in
    let()= set_msg_map_info_transient_for body transient_for in
    make_with_header ~window ~ty:MSG_MAP body

  let make_msg_wmname ~window ~wmname =
    let body = Bytes.create sizeof_msg_wmname in
    let()= Bytes.blit (Bytes.of_string wmname) 0 body 0
      (min String.(length wmname) sizeof_msg_wmname) ; (* length *) in
    make_with_header ~window ~ty:MSG_WMNAME body

  let make_msg_window_hints ~window ~width ~height =
    let body = Bytes.create sizeof_msg_window_hints in
    set_msg_window_hints_flags body Int32.(16 lor 32 |> of_int) ;
       (*^--  PMinSize | PMaxSize *)
    set_msg_window_hints_min_width body width;
    set_msg_window_hints_min_height body height;
    set_msg_window_hints_max_width body width;
    set_msg_window_hints_max_height body height;
    make_with_header ~window ~ty:MSG_WINDOW_HINTS body

  let make_msg_configure ~window ~x ~y ~width ~height =
    let body  = Bytes.create sizeof_msg_configure in
    set_msg_configure_x body x ;
    set_msg_configure_y body y ; (* x and y are from qs->window_x and window_y*)

    set_msg_configure_width body width ;
    set_msg_configure_height body height ;
    set_msg_configure_override_redirect body 0l ;
    make_with_header ~window ~ty:MSG_CONFIGURE body

  module Framing = struct
    let header_size = sizeof_msg_header
    let body_size_from_header _h =
      get_msg_header_untrusted_len _h |> Int32.to_int
  end
end

module QubesDB = struct
  [%%cenum
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
        [@@uint8_t]
  ]

  [%%cstruct
      type msg_header = {
        ty        : uint8_t;
        path      : uint8_t [@len 64];
        padding   : uint8_t [@len 3];
        data_len  : uint32_t;
        (* rest of message is data *)
      } [@@little_endian]
  ]

  let make_msg_header ~ty ~path ~data_len =
    let msg = Cstruct.create sizeof_msg_header in
    set_msg_header_ty msg (qdb_msg_to_int ty);
    Cstruct.blit_from_string path 0 (get_msg_header_path msg) 0 (String.length path);
    set_msg_header_data_len msg (Int32.of_int data_len);
    msg

  module Framing = struct
    let header_size = sizeof_msg_header
    let body_size_from_header h = get_msg_header_data_len h |> Int32.to_int
  end
end

module Rpc_filecopy = struct
  (* see qubes-linux-utils/qrexec-lib/libqubes-rpc-filecopy.h
   * and qubes-core-agent-windows/src/qrexec-services/common/filecopy.h*)
  [%%cstruct
      type file_header = {
        namelen    : uint32;
        mode       : uint32;
        filelen    : uint64;
        atime      : uint32;
        atime_nsec : uint32;
        mtime      : uint32;
        mtime_nsec : uint32;
      } [@@little_endian]
      (* followed by filename[namelen] and data[filelen] *)
  ]

  [%%cstruct
      type result_header = {
        error_code : uint32;
        _pad       : uint32;
        crc32      : uint64;
      } [@@little_endian]
  ]

  [%%cstruct
      type result_header_ext = {
        last_namelen : uint32;
        (* TODO char last_name[0]; variable length[last_namelen] *)
      } [@@little_endian]
  ]

  let make_result_header_ext last_filename =
    let namelen = Cstruct.length last_filename in
    let msg = Cstruct.create (sizeof_result_header_ext + namelen) in
    set_result_header_ext_last_namelen msg (Int32.of_int namelen);
    Cstruct.blit (* src  srcoff *) last_filename 0
                 (* dst  dstoff *) msg sizeof_result_header_ext
                 (* len *) namelen ;
    msg
end
