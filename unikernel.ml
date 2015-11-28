open Lwt

let gui_agent_port =
  match Vchan.Port.of_string "6000" with
  | `Error msg -> failwith msg
  | `Ok port -> port

module Main (C: V1_LWT.CONSOLE) = struct
  let echo ~user flow =
    Qrexec.Flow.writef flow "Hi %s! Please enter a string:" user >>= fun () ->
    Qrexec.Flow.read_line flow >>= function
    | `Eof -> return 1
    | `Ok input ->
    Qrexec.Flow.writef flow "You wrote %S. Bye." input >|= fun () -> 0

  let handler qrexec ~user cmd flow =
    (* Write a message to the client and return an exit status of 1. *)
    let error fmt =
      fmt |> Printf.ksprintf @@ fun s ->
      Qrexec.Flow.ewritef flow "%s [while processing %S]" s cmd >|= fun () -> 1 in
    match cmd with
    | "quit" when user = "root" -> Qrexec.disconnect qrexec >|= fun () -> 0
    | "quit" -> error "Permission denied"
    | "echo" -> echo ~user flow
    | cmd -> error "Unknown command %S" cmd

  let start c =
    Log.reporter := (fun lvl msg -> C.log_s c (lvl ^ ": " ^ msg));
    Qrexec.connect ~domid:0 () >>= fun qrexec ->
    let agent_listener = Qrexec.listen qrexec (handler qrexec) in
    Log.info "Starting gui-agent; waiting for client..." >>= fun () ->
    Vchan_xen.server ~domid:0 ~port:gui_agent_port () >>= fun _gui ->
    agent_listener
end
