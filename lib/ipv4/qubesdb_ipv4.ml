module Make
    (D: Qubes.S.DB)
    (R: Mirage_crypto_rng_mirage.S)
    (C: Mirage_clock.MCLOCK)
    (Ethernet : Ethernet.S)
    (Arp : Arp.S) = struct
  include Static_ipv4.Make(R)(C)(Ethernet)(Arp)
  let connect db ethif arp =
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
      let cidr = Ipaddr.V4.Prefix.make 32 ip in
      connect ~cidr ?gateway ethif arp
    | Error (`Msg m) ->
      failwith ("couldn't get ip configuration from qubesdb: " ^ m)
end
