(* Copyright (C) 2015, Thomas Leonard
   See the README file for details. *)

(** QubesOS qrexec agent protocol.
    See: https://www.qubes-os.org/doc/qrexec3/ *)

type t

module Flow : S.FLOW

module Client_flow : sig
  type t

  val write : t -> Cstruct.t -> [`Ok of unit | `Eof] Lwt.t
  (** Write to stdin *)
  val writef : t -> ('a, unit, string, [`Ok of unit | `Eof] Lwt.t) format4 -> 'a
  (* Write a formatted string to stdin *)

  val read : t -> [`Stdout of Cstruct.t | `Stderr of Cstruct.t
                  | `Eof | `Done of Cstruct.uint32] Lwt.t
  (** Read from stdout and stderr *)
end

type handler = user:string -> string -> Flow.t -> int Lwt.t
(** A handler gets a command-line and a two-way connection to the requesting
    client each time the remote client sends a MSG_EXEC_CMDLINE request. The
    "exit code" from the handler is returned to the client. *)

type client = [`Ok of Client_flow.t | `Closed | `Permission_denied | `Error of string]  -> unit Lwt.t
(** [client] is a callback for unikernel-initiated qrexec calls. The callback
    is called exactly once, and is given either a flow if successful, [`Closed]
    if the control channel has been closed, [`Permission_denied] if dom0 says
    so, or an error. *)

val connect : domid:int -> unit -> t Lwt.t
(** [connect ~domid ()] is a qrexec agent to which a client in [domid] has connected.
    Internally, it creates a server endpoint and places the endpoint information
    in XenStore, then waits for a client to connect. *)

val listen :  t -> handler -> unit Lwt.t
(** [listen t handler] is a thread that reads incoming requests from [t]
    and handles each one asynchronously with [handler]. The loop ends if
    the client disconnects. *)

val qrexec : t -> vm:string -> service:string -> client -> [`Ok | `Closed] Lwt.t
(** [qrexec t ~vm ~service ~client] initiates a qrexec call to [vm]'s service
    [service], and calls [client] with the result. If the control channel to
    dom0 is closed the result is [`Closed]. Otherwise [`Ok] is returned.
    Argument [vm] must have length less than 32 while [service] must have
    length less than 64. Otherwise [Invalid_argument] is raised. *)

val disconnect : t -> unit Lwt.t
(** Close the underlying vchan. This will cause any listening thread to finish. *)
