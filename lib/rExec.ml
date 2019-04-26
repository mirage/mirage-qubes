(* Copyright (C) 2015, Thomas Leonard
   See the README file for details. *)

open Lwt.Infix
open Formats.Qrexec
open Utils

module QV = Msg_chan.Make(Framing)

let src = Logs.Src.create "qubes.rexec" ~doc:"Qubes qrexec-agent"
module Log = (val Logs.src_log src : Logs.LOG)

type t = QV.t

let (>>!=) = Msg_chan.(>>!=)

let split chr s =
  try
    let i = String.index s chr in
    Some (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))
  with Not_found ->
    None

let or_fail = function
  | `Ok y -> return y
  | `Error (`Unknown msg) -> fail (Failure msg)
  | `Eof -> fail End_of_file

let disconnect t =
  QV.disconnect t

let vchan_base_port =
  match Vchan.Port.of_string "512" with
  | `Error msg -> failwith msg
  | `Ok port -> port

let max_data_chunk = 4096
(** Max size for data chunks. See MAX_DATA_CHUNK in qubes-linux-utils/qrexec-lib/qrexec.h *)

let rec send t ~ty data =
  let data, data' = Cstruct.split data (min max_data_chunk (Cstruct.len data)) in
  let hdr = Cstruct.create sizeof_msg_header in
  set_msg_header_ty hdr (int_of_type ty);
  set_msg_header_len hdr (Cstruct.len data |> Int32.of_int);
  if Cstruct.len data' = 0
  then QV.send t [hdr; data]
  else QV.send t [hdr; data] >>= function
    | `Eof -> return `Eof
    | `Ok () ->
      send t ~ty data'

let recv t =
  QV.recv t >>!= fun (hdr, data) ->
  let ty = get_msg_header_ty hdr |> type_of_int in
  return (`Ok (ty, data))

module Flow = struct

  type t = {
    dstream : QV.t;
    mutable input_buf : Cstruct.t;
    ty : [`Just_exec | `Exec_cmdline];
  }

  let create ~ty dstream = {dstream; input_buf = Cstruct.create 0; ty}

  let push ~stream flow buf =
    match flow.ty with
    | `Just_exec -> return ()
    | `Exec_cmdline ->
    if Cstruct.len buf > 0 then
      send flow.dstream ~ty:stream buf >>= or_fail
    else
      return ()

  let pushf ~stream flow fmt =
    fmt |> Printf.ksprintf @@ fun s ->
    push ~stream flow (Cstruct.of_string (s ^ "\n"))

  let rec read_line ~read_raw flow =
    let buf = Cstruct.to_string flow.input_buf in
    let i =
      try Some (String.index buf '\n')
      with Not_found -> None in
    match i with
    | Some i ->
        let retval = String.sub buf 0 i in
        flow.input_buf <- Cstruct.shift flow.input_buf (i + 1);
        return (`Ok retval)
    | None ->
        read_raw flow >>!= fun buf ->
        flow.input_buf <- Cstruct.append flow.input_buf buf;
        read_line ~read_raw flow

  let read ~read_raw flow =
    if Cstruct.len flow.input_buf > 0 then (
      let retval = flow.input_buf in
      flow.input_buf <- Cstruct.create 0;
      return (`Ok retval)
    ) else read_raw flow

  let close ~ty flow return_code =
    let msg = Cstruct.create sizeof_exit_status in
    set_exit_status_return_code msg (Int32.of_int return_code);
    Lwt.finalize
      (fun () ->
        send flow.dstream ~ty (Cstruct.create 0) >>!= fun () ->
        send flow.dstream ~ty:`Data_exit_code msg >>!= fun () ->
        return (`Ok ())
      )
      (fun () -> disconnect flow.dstream)
end

module Stdout : S.FLOW with type t := Flow.t = struct

  include Flow

  let write = push ~stream:`Data_stdout
  let writef fmt = pushf ~stream:`Data_stdout fmt

  let read_raw flow =
    match flow.ty with
    | `Just_exec -> return `Eof
    | `Exec_cmdline ->
    recv flow.dstream >>!= function
    | `Data_stdout, empty when Cstruct.len empty = 0 -> return `Eof
    | `Data_stdout, data -> return (`Ok data)
    | `Data_stdin, _ -> fail (error "stdin message received when we were expecting stdout - reversed flows?")
    | `Data_stderr, _ -> fail (error "stderr message received when we were expecting stdout")
    | ty, _ -> fail (error "Unknown message type %ld received" (int_of_type ty))


  let read = Flow.read ~read_raw
  let read_line = Flow.read_line ~read_raw

  let close = Flow.close ~ty:`Data_stdout
end

module Stdin : S.FLOW with type t := Flow.t = struct

  include Flow

  let write = push ~stream:`Data_stdin
  let writef fmt = pushf ~stream:`Data_stdin fmt

  let read_raw flow =
    match flow.ty with
    | `Just_exec -> return `Eof
    | `Exec_cmdline ->
    recv flow.dstream >>!= function
    | `Data_stdin, empty when Cstruct.len empty = 0 -> return `Eof
    | `Data_stdin, data -> return (`Ok data)
    | `Data_stdout, _ -> fail (error "stdout message received when we were expecting stdin - reversed flows?")
    | `Data_stderr, _ -> fail (error "stderr message received when we were expecting stdin")
    | ty, _ -> fail (error "Unknown message type %ld received" (int_of_type ty))

  let read = Flow.read ~read_raw
  let read_line = Flow.read_line ~read_raw

  let close = Flow.close ~ty:`Data_stdin
end

module Stderr : S.FLOW with type t := Flow.t = struct
  include Flow

  let write = push ~stream:`Data_stderr
  let writef fmt = pushf ~stream:`Data_stderr fmt

  let read_raw flow =
    match flow.ty with
    | `Just_exec -> return `Eof
    | `Exec_cmdline ->
    recv flow.dstream >>!= function
    | `Data_stderr, empty when Cstruct.len empty = 0 -> return `Eof
    | `Data_stderr, data -> return (`Ok data)
    | `Data_stdout, _ -> fail (error "stdout message received when we were expecting stderr")
    | `Data_stdin, _ -> fail (error "stderr message received when we were expecting stderr")
    | ty, _ -> fail (error "Unknown message type %ld received" (int_of_type ty))

  let read = Flow.read ~read_raw
  let read_line = Flow.read_line ~read_raw

  let close = Flow.close ~ty:`Data_stderr
end


type handler = user:string -> string -> Flow.t -> int Lwt.t

let send_hello t =
  let hello = Cstruct.create sizeof_peer_info in
  set_peer_info_version hello 2l;
  send t ~ty:`Hello hello >>= function
  | `Eof -> fail (error "End-of-file sending msg_hello")
  | `Ok () -> return ()

let recv_hello t =
  recv t >>= function
  | `Eof -> fail (error "End-of-file waiting for msg_hello")
  | `Ok (`Hello, resp) -> return (get_peer_info_version resp)
  | `Ok (ty, _) -> fail (error "Expected msg_hello, got %ld" (int_of_type ty))

let try_close flow return_code =
  Flow.close ~ty:`Data_stdout flow return_code >|= function
  | `Ok () -> ()
  | `Eof -> Log.warn (fun f -> f "End-of-file while closing flow (with exit status %d)" return_code)

let with_flow ~ty ~domid ~port fn =
  Lwt.try_bind
    (fun () ->
      QV.client ~domid ~port () >>= fun client ->
      Lwt.catch
        (fun () ->
          recv_hello client >>= function
          | version when version <> 2l -> fail (error "Unsupported qrexec version %ld" version)
          | _ ->
          send_hello client >|= fun () ->
          Flow.create ~ty client
        )
        (fun ex -> QV.disconnect client >>= fun () -> fail ex)
    )
    (fun flow ->
      Lwt.try_bind
        (fun () -> fn flow)
        (fun return_code -> try_close flow return_code)
        (fun ex ->
          Log.warn (fun f -> f "uncaught exception: %s" (Printexc.to_string ex));
          try_close flow 255
        )
    )
    (fun ex ->
      Log.warn (fun f -> f "Error setting up vchan (domain %d, port %s): %s"
        domid (Vchan.Port.to_string port) (Printexc.to_string ex));
      return ()
    )

let port_of_int i =
  match Int32.to_string i |> Vchan.Port.of_string with
  | `Ok p -> p
  | `Error msg -> failwith msg

let parse_cmdline cmd =
  let cmd = Cstruct.to_string cmd in
  if cmd.[String.length cmd - 1] <> '\x00' then
    fail (error "Command not null-terminated")
  else (
    let cmd = String.sub cmd 0 (String.length cmd - 1) in
    match cmd |> split ':' with
    | None -> fail (error "Missing ':' in %S" cmd)
    | Some (user, cmd) -> return (user, cmd)
  )

let exec t ~ty ~handler msg =
  Lwt.async (fun () ->
    let domid = get_exec_params_connect_domain msg |> Int32.to_int in
    let port = get_exec_params_connect_port msg |> port_of_int in
    let cmdline = Cstruct.shift msg sizeof_exec_params in
    Log.debug (fun f -> f "Execute %S" (Cstruct.to_string cmdline));
    Lwt.finalize
      (fun () ->
        with_flow ~ty ~domid ~port (fun flow ->
          parse_cmdline cmdline >>= fun (user, cmd) ->
          handler ~user cmd flow >>= fun return_code ->
          Log.debug (fun f -> f "%S returned exit status %d" cmd return_code);
          return return_code
        )
      )
      (fun () ->
        let reply = Cstruct.sub msg 0 sizeof_exec_params in
        send t ~ty:`Connection_terminated reply >|= function
        | `Ok () | `Eof -> ()
      )
  )

let listen t handler =
  let rec loop () =
    recv t >>= function
    | `Ok (`Just_exec | `Exec_cmdline as ty, data) ->
        exec t ~ty ~handler data; loop ()
    | `Ok (ty, _) ->
        Log.info (fun f -> f "unhandled qrexec message type received: %ld (%s)"
          (int_of_type ty) (string_of_type ty));
        loop ()
    | `Eof ->
        Log.info (fun f -> f "connection closed; ending listen loop");
        return `Done in
  loop () >|= fun `Done -> ()

let request_service t ~target_domain ~service_name handler =
  let request_id = "0" in
  let write_trigger_service_parameters_into buf =
    Cstruct.blit_from_string service_name 0 buf 0 (min (String.length service_name) 64);
    Cstruct.blit_from_string target_domain 0 buf 64 (min (String.length target_domain) 32);
    Cstruct.blit_from_string request_id 0 buf (64 + 32) (min (String.length request_id) 32);
    buf
  in
  let tsp = write_trigger_service_parameters_into @@ Cstruct.create sizeof_trigger_service_params in
  send t ~ty:`Trigger_service tsp >>= function
  | `Eof -> Lwt.return @@ Error `Closed
  | `Ok () ->
    let rec try_recv () =
      recv t >>= function
      | `Eof -> Lwt.return @@ Error `Closed
      | `Ok (`Service_refused, _) -> Lwt.return @@ Error `Permission_denied
      | `Ok (`Service_connect, data) -> begin
          let domid = get_exec_params_connect_domain data in
          let port = get_exec_params_connect_port data in
          Log.debug (fun f -> f "service_connect message received: domain %ld, port %ld" domid port);
          Log.debug (fun f -> f "Initiating connection...");
          (* vchan expects port to be a string that doesn't include
             several invalid characters, none of which Int.to_string should return *)
          match Vchan.Port.of_string (Int32.to_string port) with
          | `Error s -> Lwt.return @@ Error(`Msg
                                              ("failure translating vchan port number: " ^ s))
          | `Ok port ->
            QV.server ~domid:(Int32.to_int domid) ~port () >>= fun remote ->
            send_hello remote >>= fun () ->
            recv_hello remote >>= fun version ->
            Log.debug (fun f -> f "server connected on port %s, using protocol version %ld" (Vchan.Port.to_string port) version);
            let flow = Flow.create ~ty:(`Exec_cmdline) remote in
            handler ~user:"none" "" flow >>= fun _ ->
            Lwt.return (Ok ())
        end
      | `Ok (ty, _) ->
        Log.warn (fun f -> f
                     "unhandled qrexec message type received in response to \
                      trigger service request: %ld (%s)"
                     (int_of_type ty) (string_of_type ty));
        try_recv ()
    in
    try_recv ()

let connect ~domid () =
  Log.info (fun f -> f "waiting for client...");
  QV.server ~domid ~port:vchan_base_port () >>= fun t ->
  send_hello t >>= fun () ->
  recv_hello t >>= fun version ->
  Log.info (fun f -> f "client connected on port %s, using protocol version %ld" (Vchan.Port.to_string vchan_base_port) version);
  return t
