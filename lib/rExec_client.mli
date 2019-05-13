module Flow : sig
  type t

  val write : t -> Cstruct.t -> [ `Ok of unit | `Eof ] Lwt.t

  val writef : t -> ('a, unit, string, unit S.or_eof Lwt.t) format4 -> 'a

  val read : t -> [ `Stderr of Cstruct.t
                  | `Stdout of Cstruct.t
                  | `Done of int32 | `Eof ] Lwt.t

  val read_line : t -> [ `Stderr of string
                       | `Stdout of string
                       | `Done of int32 | `Eof ] Lwt.t

end

val connect : vm:string -> service:string -> identifier:string ->
  (Flow.t, [`Closed | `Permission_denied | `Msg of string ]) result Lwt.t
(** Attempt to establish a qrexec connection to the guest named [vm],
    and try to start the provided [service].
    Use [identifier] to disambiguate this traffic.
*)

val close : Flow.t -> unit Lwt.t
(** Close the underlying vchan without waiting for the remote side to complete.
    Any remaining messages will be discarded. *)
