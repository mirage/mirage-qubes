(* Copyright (C) 2015, Thomas Leonard
   See the README file for details. *)

(** The Qubes GUI agent *)

type t
type window_id
type window

open Formats.GUI

type event =
  | UNIT of unit
  | Keypress of Formats.GUI.msg_keypress_t
  | Focus of msg_focus_t
  | Motion of msg_motion_t
  | Clipboard_request
  | Clipboard_data of Cstruct.t
  | Window_crossing of msg_crossing_t
  | Window_destroy
  | Window_close
  | Button of msg_button_t

val pp_event : Format.formatter -> event -> unit
(** [pp_event formatter event] pretty-prints an event. *)

val connect : domid:int -> unit -> t Lwt.t
(** [connect domid ()] connects to the guid in the given [domid] over Vchan. *)

val listen : t -> unit -> t Lwt.t
(** [listen ti ()] is an event listener thread. It can be run with Lwt.async
                   and will never return. Events are dispatched to windows
                   created using [create_window].*)

val set_title : window -> string -> unit S.or_eof Lwt.t
val int32_of_window : window -> int32

val create_window : ?parent:window_id -> width:Cstruct.uint32 ->
                    height:Cstruct.uint32 -> t-> window S.or_eof Lwt.t
(* [create_window ?parent ~width ~height t] instantiates a new window. *)

val send : t -> Cstruct.t list -> unit S.or_eof Lwt.t
(** [send t messages] synchronously sends [messages] to the Qubes GUId
                      using [t]'s established vchan *)

val recv_event : window -> event Lwt.t
(** [recv_event] is a blocking Lwt thread that can be called repeatedly to
                 read new events coming in on [window] *)

val debug_window : window -> unit -> unit Lwt.t
(** [debug_window] is a window "handler" to be called with Lwt.async
                   that pretty-prints the received events.*)
