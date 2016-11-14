#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "mirage-qubes" @@ fun c ->
          Ok [ Pkg.mllib ~api:["Qubes"] "lib/qubes.mllib";
  ]
