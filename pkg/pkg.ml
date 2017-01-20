#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let ipv4 = Conf.(key
                 ~absent:false
                 ~doc:"Build automatic IPv4 network configurators"
                 "with-ipv4"
                 Conf.bool
)
let () =
  Pkg.describe "mirage-qubes" @@ fun c ->
  Ok [ Pkg.mllib ~api:["Qubes"] "lib/qubes.mllib";
       Pkg.mllib ~cond:(Conf.value c ipv4) "lib/ipv4/qubesdb_ipv4.mllib";
  ]
