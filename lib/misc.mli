val check_memory : unit -> [ `Ok | `Memory_critical ]
(** Check the memory situation, it triggers a garbage collection if
    free memory is low (below 40%, work-around for
    http://caml.inria.fr/mantis/view.php?id=7100 and OCaml GC needing
    to malloc extra space to run finalisers), and returns either `Ok
    or `Memory_critical if free memory is above or below 40%. *)

val shutdown : unit Lwt.t
(** Waits for `Poweroff or `Reboot from dom0. *)
