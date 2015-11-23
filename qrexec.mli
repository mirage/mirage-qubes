(** QubesOS qrexec agent protocol.
    See: https://www.qubes-os.org/doc/qrexec3/ *)

type t

module Flow : sig
  type flow

  val write : flow -> Cstruct.t -> unit Lwt.t
  (** Write to stdout *)

  val writef : flow -> ('a, unit, string, unit Lwt.t) format4 -> 'a
  (** Write a formatted line to stdout. *)

  val ewrite : flow -> Cstruct.t -> unit Lwt.t
  (** Write to stderr *)

  val ewritef : flow -> ('a, unit, string, unit Lwt.t) format4 -> 'a
  (** Write a formatted line to stderr. *)

  val read : flow -> [`Ok of Cstruct.t | `Eof] Lwt.t
  (** Read from stdin. *)

  val read_line : flow -> [`Ok of string | `Eof] Lwt.t
  (** Read a complete line from stdin. *)
end

type handler = user:string -> string -> Flow.flow -> int Lwt.t
(** A handler gets a command-line and a two-way connection to the requesting client each time
    the remote client sends a MSG_EXEC_CMDLINE request. The "exit code" from the handler
    is returned to the client. *)

val connect : domid:int -> handler:handler -> unit -> (t * int32) Lwt.t
(** Start listening on a new vchan. Place the endpoint information in XenStore and wait
    for the client to connect.
    On return, the channel is listening for incoming requests. *)

val disconnect : t -> unit Lwt.t
