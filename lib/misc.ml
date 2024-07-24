
let check_memory () =
  let fraction_free stats =
    let { Xen_os.Memory.free_words; heap_words; _ } = stats in
    float free_words /. float heap_words
  in
  let stats = Xen_os.Memory.stat () in
  if fraction_free stats > 0.4 then `Ok
  else (
    Gc.full_major ();
    let stats = Xen_os.Memory.quick_stat () in
    if fraction_free stats < 0.4 then `Memory_critical
    else `Ok
  )

let shutdown =
  let ( let* ) = Lwt.bind in
  let* value = Xen_os.Lifecycle.await_shutdown_request () in
  match value with `Poweroff | `Reboot -> Lwt.return_unit
