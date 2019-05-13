include RExec_common

open Lwt.Infix

let src = Logs.Src.create "qubes.rexec_client" ~doc:"Qubes qrexec-client"
module Log = (val Logs.src_log src : Logs.LOG)

module Flow = struct
  type t = {
    connection : QV.t;
    mutable stderr_buf : Cstruct.t;
    mutable stdout_buf : Cstruct.t;
  }

  let create connection = { connection; stderr_buf = Cstruct.empty;
                            stdout_buf = Cstruct.empty;
                          }

  let write t data = send ~ty:`Data_stdin t.connection data

  let writef t fmt =
    fmt |> Printf.ksprintf @@ fun s ->
    send ~ty:`Data_stdin t.connection (Cstruct.of_string (s ^ "\n"))

  let next_msg t =
    recv t.connection >|= function
    | `Ok (`Data_stdout, data) ->
      t.stdout_buf <- Cstruct.append t.stdout_buf data;
      `Ok t
    | `Ok (`Data_stderr, data) ->
      t.stderr_buf <- Cstruct.append t.stderr_buf data;
      `Ok t
    | `Ok (`Data_exit_code, data) ->
      `Done (Formats.Qrexec.get_exit_status_return_code data)
    | `Ok (ty, _) ->
      Log.debug Formats.Qrexec.(fun f -> f "unexpected message of type %ld (%s) received; \
                             ignoring it" (int_of_type ty) (string_of_type ty));
      `Ok t
    | `Eof -> `Eof

  let read t =
    let rec aux = function
      | `Eof | `Done _ as s -> Lwt.return s
      | `Ok t ->
        let drain_stdout () =
            let output = t.stdout_buf in
            t.stdout_buf <- Cstruct.empty;
            Lwt.return @@ `Stdout output
        and drain_stderr () =
            let output = t.stderr_buf in
            t.stderr_buf <- Cstruct.empty;
            Lwt.return @@ `Stderr output
        in
        if Cstruct.len t.stdout_buf > 0 then drain_stdout ()
        else if Cstruct.len t.stderr_buf > 0 then drain_stderr ()
        else next_msg t >>= aux
    in
    aux (`Ok t)

  let rec read_line t =
    let stdout = Cstruct.to_string t.stdout_buf
    and stderr = Cstruct.to_string t.stderr_buf
    in
    let newline buf = String.index_opt buf '\n' in
    match newline stdout, newline stderr with
    | Some i, _ ->
      let retval = String.sub stdout 0 i in
      t.stdout_buf <- Cstruct.shift t.stdout_buf (i + 1);
      Lwt.return (`Stdout retval)
    | _, Some i ->
      let retval = String.sub stderr 0 i in
      t.stderr_buf <- Cstruct.shift t.stderr_buf (i + 1);
      Lwt.return (`Stderr retval)
    | None, None ->
      next_msg t >>= function
      | `Done _ | `Eof as s -> Lwt.return s
      | `Ok t -> read_line t
end

let start_connection data =
  let domid = Formats.Qrexec.get_exec_params_connect_domain data in
  let port = Formats.Qrexec.get_exec_params_connect_port data in
  Log.debug (fun f -> f "service_connect message received: domain %ld, port %ld" domid port);
  Log.debug (fun f -> f "Connecting...");
  match Vchan.Port.of_string (Int32.to_string port) with
  | `Error msg -> Lwt.return @@ Error (`Msg msg)
  | `Ok port ->
    QV.server ~domid:(Int32.to_int domid) ~port () >>= fun remote ->
    send_hello remote >>= fun () ->
    recv_hello remote >>= fun version ->
    Log.debug (fun f -> f "server connected on port %s, using protocol version %ld" (Vchan.Port.to_string port) version);
    Lwt.return @@ Ok (Flow.create remote)

let connect ~vm ~service ~identifier =
  let write_trigger_service_parameters_into buf =
    let write_string s ~dst_offset ~max_len =
      Cstruct.blit_from_string s 0 buf dst_offset (min (String.length s) max_len)
    in
    write_string service ~dst_offset:0 ~max_len:64;
    write_string vm ~dst_offset:64 ~max_len:32;
    write_string identifier ~dst_offset:(64+32) ~max_len:32;
    buf
  in
  let tsp = write_trigger_service_parameters_into @@
    Cstruct.create Formats.Qrexec.sizeof_trigger_service_params in
  Log.debug (fun f -> f "Initiating connection to dom0 (to request service start)");
  QV.server ~domid:0 ~port:vchan_base_port () >>= fun server ->
  RExec_common.send_hello server >>= fun () ->
  RExec_common.recv_hello server >>= fun version ->
  Log.debug (fun f -> f "connection with dom0 established (version %ld)" version);
  RExec_common.send server ~ty:`Trigger_service tsp >>= function
  | `Eof -> Lwt.return @@ Error `Closed
  | `Ok () ->
    let rec try_recv () =
      recv server >>= function
      | `Eof -> Lwt.return @@ Error `Closed
      | `Ok (`Service_refused, _) -> Lwt.return @@ Error `Permission_denied
      | `Ok (`Service_connect, data) ->
        (* we have everything we need, so close the server connection *)
        QV.disconnect server >>= fun () ->
        start_connection data
      | `Ok (ty, _) ->
        let open Formats.Qrexec in
        Log.debug (fun f -> f
                     "unhandled qrexec message type received in response to \
                      trigger service request: %ld (%s)"
                     (int_of_type ty) (string_of_type ty));
        try_recv ()
    in
    try_recv ()

let close t =
  QV.disconnect t.Flow.connection
