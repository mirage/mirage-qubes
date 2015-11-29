(* Copyright (C) 2015, Thomas Leonard
   See the README file for details. *)

open Lwt

module Main (C: V1_LWT.CONSOLE) (Clock : V1.CLOCK) = struct
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

  let wait_for_shutdown () =
    let module Xs = OS.Xs in
    Xs.make () >>= fun xs ->
    Xs.immediate xs (fun h -> Xs.read h "domid") >>= fun domid ->
    let domid = int_of_string domid in
    Xs.immediate xs (fun h -> Xs.getdomainpath h domid) >>= fun domainpath ->
    Xs.wait xs (fun xsh ->
      Xs.read xsh (domainpath ^ "/control/shutdown") >>= function
      | "poweroff" -> return `Poweroff
      | "" -> fail Xs_protocol.Eagain
      | state ->
          Log.info "Unknown power state %S" state;
          fail Xs_protocol.Eagain
    )

  let start c () =
    let start_time = Clock.time () in
    Log.reporter := (fun lvl msg ->
      let now = Clock.time () |> Gmtime.gmtime |> Gmtime.to_string in
      C.log c (Printf.sprintf "%s: %s: %s" now lvl msg)
    );
    (* Start qrexec agent and GUI agent in parallel *)
    let qrexec = Qrexec.connect ~domid:0 () in
    let gui = Gui.connect ~domid:0 () in
    (* Wait for clients to connect *)
    qrexec >>= fun qrexec ->
    let agent_listener = Qrexec.listen qrexec (handler qrexec) in
    gui >>= fun _gui ->
    Log.info "agents connected in %.3f s (CPU time used since boot: %.3f s)"
      (Clock.time () -. start_time) (Sys.time ());
    Lwt.async (fun () ->
      wait_for_shutdown () >>= fun `Poweroff ->
      Qrexec.disconnect qrexec
    );
    agent_listener
end
