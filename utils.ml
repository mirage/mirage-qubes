let error fmt =
  let err s = Failure s in
  Printf.ksprintf err fmt

let return = Lwt.return
let fail = Lwt.fail

(* Copy str to the start of buffer and fill the rest with zeros *)
let set_fixed_string buffer str =
  let len = String.length str in
  Cstruct.blit_from_string str 0 buffer 0 len;
  Cstruct.memset (Cstruct.shift buffer len) 0
