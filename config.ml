open Mirage

let main =
  let vchan_libraries =
    match get_mode () with
    | `Xen -> ["vchan.xen"]
    | `Unix -> ["vchan.lwt"] in
  foreign
    ~libraries:vchan_libraries
    ~packages:["vchan"; "cstruct"]
    "Unikernel.Main" (console @-> clock @-> job)

let () =
  register "qubes-test" [main $ default_console $ default_clock]
