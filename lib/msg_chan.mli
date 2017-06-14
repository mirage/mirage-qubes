(* Copyright (C) 2015, Thomas Leonard
   See the README file for details. *)

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
