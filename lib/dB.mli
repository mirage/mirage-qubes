(* Copyright (C) 2015, Thomas Leonard
   See the README file for details. *)

(** A QubesDB client *)

include S.DB

val connect : domid:int -> unit -> t Lwt.t
(** [connect ~domid ()] is a QubesDB agent which connects to a server in [domid]. *)

val disconnect : t -> unit Lwt.t
(** Close the underlying vchan. *)
