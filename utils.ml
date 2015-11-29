let error fmt =
  let err s = Failure s in
  Printf.ksprintf err fmt

let return = Lwt.return
let fail = Lwt.fail
