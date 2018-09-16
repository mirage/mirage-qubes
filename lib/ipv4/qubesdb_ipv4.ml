module Make
    (D: Qubes.S.DB)
    (R: Mirage_random.C)
    (C: Mirage_clock.MCLOCK)
    (Ethernet : Mirage_protocols_lwt.ETHIF)
    (Arp : Mirage_protocols_lwt.ARP) = struct
  include Static_ipv4.Make(R)(C)(Ethernet)(Arp)
  let connect db clock ethif arp =
    let (>>=?) ip f = match ip with
      | None -> None
      | Some s -> f s
    in
    let ip = D.read db "/qubes-ip" >>=? Ipaddr.V4.of_string in
    let gateway = D.read db "/qubes-gateway" >>=? Ipaddr.V4.of_string in
    match ip with
    | Some ip ->
      let network = Ipaddr.V4.Prefix.make 32 ip in
      connect ~ip ~network ~gateway clock ethif arp
    | _ -> Lwt.fail_with "couldn't get ip configuration from qubesdb"
end
