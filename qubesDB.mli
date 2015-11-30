(* Copyright (C) 2015, Thomas Leonard
   See the README file for details. *)

(** A QubesDB client *)

type t

val connect : domid:int -> unit -> t Lwt.t
(** [connect ~domid ()] is a QubesDB agent which connects to a server in [domid]. *)

val get :  t -> string -> string option
(** [get t key] is the last known value of [key]. *)

val disconnect : t -> unit Lwt.t
(** Close the underlying vchan. *)
