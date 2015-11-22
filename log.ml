let boot_reporter lvl s = Printf.printf "%s: %s\n%!" lvl s; Lwt.return ()
let reporter = ref boot_reporter

let info fmt =
  Printf.ksprintf (!reporter "info") fmt

let warn fmt =
  Printf.ksprintf (!reporter "WARN") fmt
