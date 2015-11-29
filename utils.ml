let error fmt =
  let err s = Failure s in
  Printf.ksprintf err fmt
