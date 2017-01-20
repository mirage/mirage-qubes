module Make(D : Qubes.S.DB)
           (Ethernet : Mirage_protocols_lwt.ETHIF)
           (Arp : Mirage_protocols_lwt.ARP) : sig

  include Mirage_protocols_lwt.IPV4 with type ethif = Ethernet.t
  val connect : D.t -> Ethernet.t -> Arp.t -> t io
  (** [connect db ethernet arp] attempts to use the provided [db] 
   *  to look up the correct IPV4 information, and construct
   *  an ipv4 implementation based on [ethernet] and [arp].  If [db]
   *  can't be read or doesn't contain useful values, [connect] will
   *  raise a failure. *)
end
