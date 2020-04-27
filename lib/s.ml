type 'a or_eof =
  [ `Ok of 'a
  | `Eof ]

module type MSG_CHAN = sig
  type t

  val recv : t -> (Cstruct.t * Cstruct.t) or_eof Lwt.t
  (** Receive one packet (header and body) *)

  val recv_fixed : t -> int -> Cstruct.t or_eof Lwt.t
  (** Receive one packet of known size (no header) *)

  val recv_raw : t -> Cstruct.t or_eof Lwt.t
  (** Read a chunk of data from the stream.
      Blocks if no data is available yet. *)

  val send : t -> Cstruct.t list -> unit or_eof Lwt.t
  (** Send one or more packets (takes the mutex) *)
end

module type FLOW = sig
  type t

  val write : t -> Cstruct.t -> unit Lwt.t
  (** Write to stdout *)

  val writef : t -> ('a, unit, string, unit Lwt.t) format4 -> 'a
  (** Write a formatted line to stdout. *)

  val ewrite : t -> Cstruct.t -> unit Lwt.t
  (** Write to stderr *)

  val ewritef : t -> ('a, unit, string, unit Lwt.t) format4 -> 'a
  (** Write a formatted line to stderr. *)

  val read : t -> [`Ok of Cstruct.t | `Eof] Lwt.t
  (** Read from stdin. *)

  val read_line : t -> [`Ok of string | `Eof] Lwt.t
  (** Read a complete line from stdin. *)
end

module type DB = sig
  type t

  module KeyMap : Map.S with type key = string

  val values : t -> string -> string KeyMap.t
  (** [values t key] is the database that contains all values whose prefix
     is [key]. *)

  val read :  t -> string -> string option
  (** [read t key] is the last known value of [key]. *)

  val write :  t -> string -> string -> unit Lwt.t
  (** [write t key value] sets [key] to [value]. *)

  val bindings : t -> string KeyMap.t
  (** [bindings t] is the bindings of [t] at the time of the call. *)

  val after : t -> string KeyMap.t -> string KeyMap.t Lwt.t
  (** [after prev] waits until the current bindings are different to [prev]
      and then returns the new bindings. *)

  val got_new_commit : t -> string -> string KeyMap.t Lwt.t
  (** [got_new_commit t key] waits until a new commit (empty write) has been
      written to [key] into the qubesDB [t]. Returns the entries starting with
      [key]. *)
end
