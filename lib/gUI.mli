(* Copyright (C) 2015, Thomas Leonard
   See the README file for details. *)

(** The Qubes GUI agent *)

type t

val connect : domid:int -> unit -> t Lwt.t
