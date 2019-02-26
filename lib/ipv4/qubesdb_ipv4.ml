module Make
    (D: Qubes.S.DB)
    (R: Mirage_random.C)
    (C: Mirage_clock.MCLOCK)
    (Ethernet : Mirage_protocols_lwt.ETHERNET)
    (Arp : Mirage_protocols_lwt.ARP) = struct
  include Static_ipv4.Make(R)(C)(Ethernet)(Arp)
  let connect db clock ethif arp =
    let (>>=?) ip f = match ip with
      | None -> Error (`Msg "couldn't read qubesdb")
      | Some s -> f s
    in
    let ip = D.read db "/qubes-ip" >>=? Ipaddr.V4.of_string in
    let gateway = match D.read db "/qubes-gateway" >>=? Ipaddr.V4.of_string with
      | Ok ip -> Some ip
      | Error _ -> None
    in
    match ip with
    | Ok ip ->
      let network = Ipaddr.V4.Prefix.make 32 ip in
      connect ~ip ~network ~gateway clock ethif arp
    | Error (`Msg m) ->
      Lwt.fail_with ("couldn't get ip configuration from qubesdb: " ^ m)
end
