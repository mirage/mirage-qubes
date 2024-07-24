val check_memory : ?fraction:int -> unit -> [ `Ok | `Memory_critical ]
(** Check the memory situation, it triggers a garbage collection if
    free memory is low (below [fraction]%, work-around for
    http://caml.inria.fr/mantis/view.php?id=7100 and OCaml GC needing
    to malloc extra space to run finalisers), and returns either `Ok
    or `Memory_critical if free memory is above or below [fraction]%. *)

val shutdown : unit Lwt.t
(** Waits for `Poweroff or `Reboot from dom0. *)
