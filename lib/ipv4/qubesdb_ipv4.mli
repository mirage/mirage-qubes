module Make
    (D : Qubes.S.DB)
    (R : Mirage_random.S)
    (C : Mirage_clock.MCLOCK)
    (Ethernet : Ethernet.S)
    (Arp : Arp.S) : sig

  include Tcpip.Ip.S with type ipaddr = Ipaddr.V4.t and type prefix = Ipaddr.V4.Prefix.t
  val connect : D.t -> Ethernet.t -> Arp.t -> t Lwt.t
  (** [connect db ethernet arp] attempts to use the provided [db]
   *  to look up the correct IPV4 information, and construct
   *  an ipv4 implementation based on [ethernet] and [arp].  If [db]
   *  can't be read or doesn't contain useful values, [connect] will
   *  raise a failure. *)
end
