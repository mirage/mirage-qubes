(** A Qubes notification proxy client. Replies are ignored. *)

type t

val connect : ?vm:string -> RExec.t -> (t, string) result Lwt.t
(** [connect rexec] calls the qubes.Notifications service and negotiates the protocol version.
    [vm] serves as prefix for futures notifications (default empty). *)

val notify : t -> ?body:string -> string -> (unit, string) result Lwt.t
(** [notify t ~body summary] sends a notification. Non-valid UTF8 chars are replaced by '?'. *)

(** Encoding, no I/O. *)
module Wire : sig
  val negotiate : string -> (string, string) result
  (** Takes the 4 bytes sent first by the server, returns the answer. *)

  val encode : sequence:int64 -> summary:string -> body:string -> string
  (** Framed message: u32 length then the bincode body. *)
end
