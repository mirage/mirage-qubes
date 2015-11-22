open Lwt

let split chr s =
  try
    let i = String.index s chr in
    Some (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))
  with Not_found ->
    None

module Main (C: V1_LWT.CONSOLE) = struct
  let exit_requested, exit_waker = Lwt.wait ()

  let echo ~user flow =
    Qrexec.Flow.writef flow "Hi %s! Please enter a string:" user >>= fun () ->
    Qrexec.Flow.read_line flow >>= function
    | `Eof -> return 1
    | `Ok input ->
    Qrexec.Flow.writef flow "You wrote %S. Bye." input >|= fun () -> 0

  let handler cmd flow =
    (* Write a message to the client and return an exit status of 1. *)
    let error fmt =
      fmt |> Printf.ksprintf @@ fun s ->
      Qrexec.Flow.ewritef flow "%s [while processing %S]" s cmd >|= fun () -> 1 in
    match cmd |> split ':' with
    | None -> error "Missing ':' in %S" cmd
    | Some (user, cmd) ->
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
    exit_requested >>= fun () ->
    Log.info "Closing server..." >>= fun () ->
    Qrexec.disconnect qrexec
end
