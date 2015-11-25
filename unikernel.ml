open Lwt

let gui_agent_port =
  match Vchan.Port.of_string "6000" with
  | `Error msg -> failwith msg
  | `Ok port -> port

module Main (C: V1_LWT.CONSOLE) = struct
  let exit_requested, exit_waker = Lwt.wait ()

  let echo ~user flow =
    Qrexec.Flow.writef flow "Hi %s! Please enter a string:" user >>= fun () ->
    Qrexec.Flow.read_line flow >>= function
    | `Eof -> return 1
    | `Ok input ->
    Qrexec.Flow.writef flow "You wrote %S. Bye." input >|= fun () -> 0

  let handler ~user cmd flow =
    (* Write a message to the client and return an exit status of 1. *)
    let error fmt =
      fmt |> Printf.ksprintf @@ fun s ->
      Qrexec.Flow.ewritef flow "%s [while processing %S]" s cmd >|= fun () -> 1 in
    match cmd with
    | "quit" ->
        if user = "root" then (Lwt.wakeup exit_waker (); return 0)
        else error "Permission denied"
    | "echo" -> echo ~user flow
    | cmd -> error "Unknown command %S" cmd

  let start c =
    Log.reporter := (fun lvl msg -> C.log_s c (lvl ^ ": " ^ msg));
    Qrexec.connect ~handler ~domid:0 () >>= fun (qrexec, client_version) ->
    Log.info "Handshake done; client version is %ld" client_version >>= fun () ->
    Log.info "Starting gui-agent; waiting for client..." >>= fun () ->
    Vchan_xen.server ~domid:0 ~port:gui_agent_port () >>= fun _gui ->
    exit_requested >>= fun () ->
    Log.info "Closing server..." >>= fun () ->
    Qrexec.disconnect qrexec
end
