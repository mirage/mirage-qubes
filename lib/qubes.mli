(* Copyright (C) 2015, Thomas Leonard
   See the README file for details. *)

module S : sig
  type 'a or_eof =
    [ `Ok of 'a
    | `Eof ]

  module type MSG_CHAN = sig
    type t

    val recv : t -> (Cstruct.t * Cstruct.t) or_eof Lwt.t
    (** Receive one packet (header and body) *)

    val recv_fixed : t -> int -> Cstruct.t or_eof Lwt.t
    (** Receive one packet of known size (no header) *)

    val recv_raw : t -> Cstruct.t or_eof Lwt.t
    (** Read a chunk of data from the stream.
        Blocks if no data is available yet. *)

    val send : t -> Cstruct.t list -> unit or_eof Lwt.t
    (** Send one or more packets (takes the mutex) *)
  end

  module type FLOW = sig
    type t

    val write : t -> Cstruct.t -> unit Lwt.t
    (** Write to stdout *)

    val writef : t -> ('a, unit, string, unit Lwt.t) format4 -> 'a
    (** Write a formatted line to stdout. *)

    val ewrite : t -> Cstruct.t -> unit Lwt.t
    (** Write to stderr *)

    val ewritef : t -> ('a, unit, string, unit Lwt.t) format4 -> 'a
    (** Write a formatted line to stderr. *)

    val read : t -> [`Ok of Cstruct.t | `Eof] Lwt.t
    (** Read from stdin. *)

    val read_line : t -> [`Ok of string | `Eof] Lwt.t
    (** Read a complete line from stdin. *)
  end
  module type DB = sig
    type t

    module KeyMap : Map.S with type key = string

    val read :  t -> string -> string option
    (** [read t key] is the last known value of [key]. *)

    val write :  t -> string -> string -> unit Lwt.t
    (** [write t key value] sets [key] to [value]. *)

    val bindings : t -> string KeyMap.t
    (** [bindings t] is the bindings of [t] at the time of the call. *)

    val after : t -> string KeyMap.t -> string KeyMap.t Lwt.t
    (** [after prev] waits until the current bindings are different to [prev]
        and then returns the new bindings. *)
  end
end

module GUI : sig
  (** The Qubes GUI agent *)

  type t

  val connect : domid:int -> unit -> t Lwt.t

  val listen : t -> 'a Lwt.t
end

module DB : sig
  (** A QubesDB client *)

  include S.DB

  val connect : domid:int -> unit -> t Lwt.t
  (** [connect ~domid ()] is a QubesDB agent which connects to a server in [domid]. *)

  val disconnect : t -> unit Lwt.t
  (** Close the underlying vchan. *)
end

module Msg_chan : sig
  (** A message-based protocol on top of VChan streams. *)

  val (>>!=) : 'a S.or_eof Lwt.t -> ('a -> ([> `Eof] as 'b) Lwt.t) -> 'b Lwt.t

  module Make (F : Formats.FRAMING) : sig
    include S.MSG_CHAN

    val client : domid:int -> port:Vchan_xen.port -> unit -> t Lwt.t
    (** Create a client *)

    val server : domid:int -> port:Vchan_xen.port -> unit -> t Lwt.t
    (** Create a server *)

    val disconnect : t -> unit Lwt.t
  end
end

module RExec : sig
  (** QubesOS qrexec agent protocol.
      See: https://www.qubes-os.org/doc/qrexec3/ *)

  type t

  module Flow : S.FLOW

  type handler = user:string -> string -> Flow.t -> int Lwt.t
  (** A handler gets a command-line and a two-way connection to the requesting client each time
      the remote client sends a MSG_EXEC_CMDLINE request. The "exit code" from the handler
      is returned to the client. *)

  val connect : domid:int -> unit -> t Lwt.t
  (** [connect ~domid ()] is a qrexec agent to which a client in [domid] has connected.
      Internally, it creates a server endpoint and places the endpoint information
      in XenStore, then waits for a client to connect. *)

  val listen :  t -> handler -> unit Lwt.t
  (** [listen t handler] is a thread that reads incoming requests from [t]
      and handles each one asynchronously with [handler]. The loop ends if
      the client disconnects. *)

  val disconnect : t -> unit Lwt.t
  (** Close the underlying vchan. This will cause any listening thread to finish. *)
end
