(* Copyright (C) 2015, Thomas Leonard
   See the README file for details. *)

(** A message-based protocol on top of VChan streams. *)

type 'a or_eof =
  [ `Ok of 'a
  | `Eof ]

val (>>!=) : 'a or_eof Lwt.t -> ('a -> ([> `Eof] as 'b) Lwt.t) -> 'b Lwt.t

module Make (F : Qubes_protocol.FRAMING) : sig
  type t

  val recv : t -> (Cstruct.t * Cstruct.t) or_eof Lwt.t
  (** Receive one packet (header and body) *)

  val recv_fixed : t -> int -> Cstruct.t or_eof Lwt.t
  (** Receive one packet of known size (no header) *)

  val send : t -> Cstruct.t list -> unit or_eof Lwt.t
  (** Send one or more packets (takes the mutex) *)

  val client : domid:int -> port:Vchan_xen.port -> unit -> t Lwt.t
  (** Create a client *)

  val server : domid:int -> port:Vchan_xen.port -> unit -> t Lwt.t
  (** Create a server *)

  val disconnect : t -> unit Lwt.t
end
