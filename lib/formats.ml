(** The Qubes wire protocol details. *)
(** for more details, see qubes-gui-common/include/qubes-gui-protocol.h *)

open Utils

module type FRAMING = sig
  val header_size : int
  val body_size_from_header : Cstruct.t -> int
end

module Qrexec = struct
  [%%cstruct
      type msg_header = {
        ty : uint32_t;
        len : uint32_t;
      } [@@little_endian]
  ]

  [%%cstruct
      type peer_info = {
        version : uint32_t;
      } [@@little_endian]
  ]

  [%%cstruct
      type exec_params = {
        connect_domain : uint32_t;
        connect_port : uint32_t;
        (* rest of message is command line *)
      } [@@little_endian]
  ]

  [%%cstruct
      type exit_status = {
        (* XXX: size of message depends on native int size?? *)
        return_code : uint32_t;
      } [@@little_endian]
  ]

  type msg_type =
    [ `Exec_cmdline
    | `Just_exec
    | `Service_connect
    | `Trigger_service
    | `Connection_terminated
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
    | 0x210l -> `Trigger_service
    | 0x211l -> `Connection_terminated
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
    | `Trigger_service -> 0x210l
    | `Connection_terminated -> 0x211l
    | `Hello -> 0x300l
    | `Unknown x -> x

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
  [%%cstruct
      type msg_header = {
        ty : uint32_t; (** type *)
        window : uint32_t;
        untrusted_len : uint32_t;
      } [@@little_endian]
  ]

  (** VM -> Dom0, Dom0 -> VM *)
  [%%cstruct
      type msg_map_info = {
        override_redirect : uint32_t;
        transient_for     : uint32_t;
      } [@@little_endian]
  ]

  (** Dom0 -> VM, dom0 wants us to reply with a MSG_CLIPBOARD_DATA *)
  [%%cstruct
      type msg_clipboard_req = {
        empty : uint8_t [@len 0]
      } [@@little_endian]
  ]

  (** Dom0 -> VM, VM -> Dom0: MSG_CLIPBOARD_DATA:*)
  (** a normal header, followed by a uint8 array of size len *)

  (** VM -> Dom0 *)
  [%%cstruct
      type msg_create = {
        x      : uint32_t; (* position of window, seems to be converted *)
        y      : uint32_t;
        width  : uint32_t;
        height : uint32_t; (* from qubes src: "size of image" *)
        parent : uint32_t;
        override_redirect : uint32_t;
      } [@@little_endian]
  ]

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
  [%%cstruct
      type msg_configure = {
        x      : uint32_t;
        y      : uint32_t;
        width  : uint32_t;
        height : uint32_t;
        override_redirect : uint32_t;
      } [@@little_endian]
  ]

  (** VM -> Dom0 *)
  [%%cstruct
   type msg_shmimage = {
     x : uint32_t;
     y : uint32_t;
     width : uint32_t;
     height: uint32_t;
   } [@@little_endian]
  ]

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
  [%%cstruct
   type msg_window_hints = {
     flags : uint32_t;
     min_width : uint32_t;
     min_height: uint32_t;
     max_width: uint32_t;
     max_height: uint32_t;
     width_inc: uint32_t;
     height_inc: uint32_t;
     base_width: uint32_t;
     base_height: uint32_t;
   } [@@little_endian]
  ]

  (** VM -> Dom0, Dom0 -> VM *)
  [%%cstruct
   type msg_window_flags = {
     (* &1= FULLSCREEN, &2= DEMANDS_ATTENTION, &4=MINIMIZE *)
     flags_set   : uint32_t;
     flags_unset : uint32_t;
   } [@@little_endian]
  ]

  (** VM -> Dom0 *)
  [%%cstruct
      type shm_cmd = {
        shmid     : uint32_t;
        width     : uint32_t;
        height    : uint32_t;
        bpp       : uint32_t; (* bpp = bits per pixel *)
        off       : uint32_t;
        num_mfn   : uint32_t; (* number of pixels *)
        domid     : uint32_t;
        (* followed by a variable length buffer of pixels:*)
        (* uint32_t mfns[0]; *)
      } [@@little_endian]
  ]

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
    let body_len = Cstruct.len body in
    let msg = Cstruct.create (sizeof_msg_header + body_len) in
    let()= set_msg_header_ty     msg (msg_type_to_int ty) in
    let()= set_msg_header_window msg window in
    let()= set_msg_header_untrusted_len msg Int32.(of_int body_len) in
    let() = Cstruct.blit
        (* src, srcoff: *) body 0
        (* dst, dstoff: *) msg sizeof_msg_header
        (* length: *)      Cstruct.(len body)
    in msg

  let make_msg_mfndump ~window ~width ~height ~mfns =
    (* n.b. must be followed by a MSG_SHMIMAGE to actually repaint *)
    let num_mfn = List.length mfns in
    let offset  = 0x0l in
    let body = Cstruct.create (sizeof_shm_cmd + num_mfn*4) in
    set_shm_cmd_width   body width;
    set_shm_cmd_height  body height;
    set_shm_cmd_bpp     body 24l; (* bits per pixel *)
    set_shm_cmd_off     body offset;
    set_shm_cmd_num_mfn body Int32.(of_int num_mfn);
    (* From https://www.qubes-os.org/doc/gui/
       >> "shmid" and "domid" parameters are just placeholders (to be filled
       >> by *qubes_guid* ), so that we can use the same structure when talking
       >> to shmoverride.so **)
    (* set_shm_cmd_domid   body Int32.(of_int domid);
       set_shm_cmd_shmid   body 0l; *)

    (* TODO let n = (4 * width * height + offset
                     + (XC_PAGE_SIZE-1)) / XC_PAGE_SIZE; *)
    mfns |> List.iteri (fun i ->
        Cstruct.LE.set_uint32 body (sizeof_shm_cmd + i*4));
    make_with_header ~window ~ty:MSG_MFNDUMP body

  let make_msg_shmimage ~window ~x ~y ~width ~height =
    let body = Cstruct.create (sizeof_msg_shmimage) in
    set_msg_shmimage_x body x;
    set_msg_shmimage_y body y;
    set_msg_shmimage_width body width;
    set_msg_shmimage_height body height;
    make_with_header ~window ~ty:MSG_SHMIMAGE body

  let make_msg_create ~window ~width ~height ~x ~y ~override_redirect ~parent =
    let body = Cstruct.create sizeof_msg_create in
    set_msg_create_width             body width; (*  w *)
    set_msg_create_height            body height; (* h *)
    set_msg_create_x                 body x;
    set_msg_create_y                 body y;
    set_msg_create_override_redirect body override_redirect;
    set_msg_create_parent            body parent;
    make_with_header ~window ~ty:MSG_CREATE body

  let make_msg_map_info ~window ~override_redirect ~transient_for =
    let body = Cstruct.create sizeof_msg_map_info in
    let()= set_msg_map_info_override_redirect body override_redirect in
    let()= set_msg_map_info_transient_for body transient_for in
    make_with_header ~window ~ty:MSG_MAP body

  let make_msg_wmname ~window ~wmname =
    let body = Cstruct.create sizeof_msg_wmname in
    let()= Cstruct.blit_from_string wmname 0 body 0
      (min String.(length wmname) sizeof_msg_wmname) ; (* length *) in
    make_with_header ~window ~ty:MSG_WMNAME body

  let make_msg_window_hints ~window ~width ~height =
    let body = Cstruct.create sizeof_msg_window_hints in
    set_msg_window_hints_flags body Int32.(16 lor 32 |> of_int) ;
       (*^--  PMinSize | PMaxSize *)
    set_msg_window_hints_min_width body width;
    set_msg_window_hints_min_height body height;
    set_msg_window_hints_max_width body width;
    set_msg_window_hints_max_height body height;
    make_with_header ~window ~ty:MSG_WINDOW_HINTS body

  let make_msg_configure ~window ~x ~y ~width ~height =
    let body  = Cstruct.create sizeof_msg_configure in
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
    set_fixed_string (get_msg_header_path msg) path;
    Cstruct.memset (get_msg_header_padding msg) 0;
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
    let namelen = Cstruct.len last_filename in
    let msg = Cstruct.create (sizeof_result_header_ext + namelen) in
    set_result_header_ext_last_namelen msg (Int32.of_int namelen);
    Cstruct.blit (* src  srcoff *) last_filename 0
                 (* dst  dstoff *) msg sizeof_result_header_ext
                 (* len *) namelen ;
    msg
end
