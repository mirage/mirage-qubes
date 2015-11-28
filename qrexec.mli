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
