(* Copyright (C) 2015, Thomas Leonard
   See the README file for details. *)

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

val request_service : t -> target_domain:string -> service_name:string -> handler ->
  (unit, [`Closed | `Permission_denied | `Msg of string ]) result Lwt.t
(** [request_service t domain service ident handler] requests
    via [t] that [domain] start a [service].
    If the service is requested successfully, `handler` will be invoked
    with the message flow.
    [request_service] does not multiplex incoming messages based on a unique
    identifier, so at most one active channel should be open between two domains
    at a time.
*)

val disconnect : t -> unit Lwt.t
(** Close the underlying vchan. This will cause any listening thread to finish. *)
