module Make
    (D : Qubes.S.DB)
    (R : Mirage_random.C)
    (C : Mirage_clock.MCLOCK)
    (Ethernet : Mirage_protocols_lwt.ETHERNET)
    (Arp : Mirage_protocols_lwt.ARP) : sig

  include Mirage_protocols_lwt.IPV4
  val connect : D.t -> C.t -> Ethernet.t -> Arp.t -> t io
  (** [connect db clock ethernet arp] attempts to use the provided [db]
   *  to look up the correct IPV4 information, and construct
   *  an ipv4 implementation based on [clock], [ethernet] and [arp].  If [db]
   *  can't be read or doesn't contain useful values, [connect] will
   *  raise a failure. *)
end
