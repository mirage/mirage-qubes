module Make
    (D : Qubes.S.DB)
    (R : Mirage_random.S)
    (C : Mirage_clock.MCLOCK)
    (Ethernet : Mirage_protocols.ETHERNET)
    (Arp : Mirage_protocols.ARP) : sig

  include Mirage_protocols.IPV4
  val connect : D.t -> Ethernet.t -> Arp.t -> t Lwt.t
  (** [connect db ethernet arp] attempts to use the provided [db]
   *  to look up the correct IPV4 information, and construct
   *  an ipv4 implementation based on [ethernet] and [arp].  If [db]
   *  can't be read or doesn't contain useful values, [connect] will
   *  raise a failure. *)
end
