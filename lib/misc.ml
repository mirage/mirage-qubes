
let check_memory ?(fraction=40) () =
  if fraction <= 0 || fraction >= 100 then invalid_arg "fraction invalid: 0 < fraction < 100";
  let is_enough stats =
    let { Xen_os.Memory.free_words; heap_words; _ } = stats in
    (* Assuming 64bits integers, the following should not overlap *)
    free_words * 100 >  heap_words * fraction
  in
  let stats = Xen_os.Memory.stat () in
  if is_enough stats then `Ok
  else (
    Gc.full_major ();
    let stats = Xen_os.Memory.quick_stat () in
    if is_enough stats then `Ok
    else `Memory_critical
  )

let shutdown =
  let ( let* ) = Lwt.bind in
  let* value = Xen_os.Lifecycle.await_shutdown_request () in
  match value with `Poweroff | `Reboot -> Lwt.return_unit
