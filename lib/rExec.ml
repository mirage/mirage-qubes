(* Copyright (C) 2015, Thomas Leonard
   See the README file for details. *)

open Lwt.Infix
open Formats.Qrexec
open Utils

module QV = Msg_chan.Make(Framing)

let src = Logs.Src.create "qubes.rexec" ~doc:"Qubes qrexec-agent"
module Log = (val Logs.src_log src : Logs.LOG)

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
    mutable stdin_buf : Cstruct.t;
    ty : [`Just_exec | `Exec_cmdline];
  }

  let create ~ty dstream = {dstream; stdin_buf = Cstruct.create 0; ty}

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

  let write = push ~stream:`Data_stdout
  let ewrite = push ~stream:`Data_stderr

  let writef fmt = pushf ~stream:`Data_stdout fmt
  let ewritef fmt = pushf ~stream:`Data_stderr fmt

  let read_raw flow =
    match flow.ty with
    | `Just_exec -> return `Eof
    | `Exec_cmdline ->
    recv flow.dstream >>!= function
    | `Data_stdin, empty when Cstruct.len empty = 0 -> return `Eof
    | `Data_stdin, data -> return (`Ok data)
    | ty, _ -> fail (error "Unknown message type %ld received" (int_of_type ty))

  let read flow =
    if Cstruct.len flow.stdin_buf > 0 then (
      let retval = flow.stdin_buf in
      flow.stdin_buf <- Cstruct.create 0;
      return (`Ok retval)
    ) else read_raw flow

  let rec read_line flow =
    let buf = Cstruct.to_string flow.stdin_buf in
    let i =
      try Some (String.index buf '\n')
      with Not_found -> None in
    match i with
    | Some i ->
        let retval = String.sub buf 0 i in
        flow.stdin_buf <- Cstruct.shift flow.stdin_buf (i + 1);
        return (`Ok retval)
    | None ->
        read_raw flow >>!= fun buf ->
        flow.stdin_buf <- Cstruct.append flow.stdin_buf buf;
        read_line flow

  let close flow return_code =
    let msg = Cstruct.create sizeof_exit_status in
    set_exit_status_return_code msg (Int32.of_int return_code);
    Lwt.finalize
      (fun () ->
        send flow.dstream ~ty:`Data_stdout (Cstruct.create 0) >>!= fun () ->
        send flow.dstream ~ty:`Data_exit_code msg >>!= fun () ->
        return (`Ok ())
      )
      (fun () -> QV.disconnect flow.dstream)
end

module Client_flow = struct
  type t = {
    dstream : QV.t;
    mutable stdout_buf : Cstruct.t;
    mutable stderr_buf : Cstruct.t;
  }

  let create dstream = { dstream; stdout_buf = Cstruct.empty;
                         stderr_buf = Cstruct.empty }

  let write t data = send ~ty:`Data_stdin t.dstream data

  let writef t fmt =
    fmt |> Printf.ksprintf @@ fun s ->
    send ~ty:`Data_stdin t.dstream (Cstruct.of_string s)

  let next_msg t =
    recv t.dstream >|= function
    | `Ok (`Data_stdout, data) ->
      t.stdout_buf <- Cstruct.append t.stdout_buf data;
      `Ok t
    | `Ok (`Data_stderr, data) ->
      t.stderr_buf <- Cstruct.append t.stderr_buf data;
      `Ok t
    | `Ok (`Data_exit_code, data) ->
      `Exit_code (Formats.Qrexec.get_exit_status_return_code data)
    | `Ok (ty, _) ->
      Log.err Formats.Qrexec.(fun f -> f "unexpected message of type %ld (%s) received; \
                                          ignoring it" (int_of_type ty) (string_of_type ty));
      `Ok t
    | `Eof -> `Eof

  let read t =
    let rec aux = function
      | `Eof | `Exit_code _ as s -> Lwt.return s
      | `Ok t ->
        let drain_stdout () =
          let output = t.stdout_buf in
          t.stdout_buf <- Cstruct.empty;
          Lwt.return (`Stdout output)
        and drain_stderr () =
          let output = t.stderr_buf in
          t.stderr_buf <- Cstruct.empty;
          Lwt.return (`Stderr output)
        in
        if Cstruct.len t.stdout_buf > 0
        then drain_stdout ()
        else if Cstruct.len t.stderr_buf > 0
        then drain_stderr ()
        else next_msg t >>= aux
    in
    aux (`Ok t)
end

type identifier = string
(** [identifier] is used to distinguish client qrexec calls *)

type client = [`Ok of Client_flow.t | `Closed | `Permission_denied | `Error of string] -> unit Lwt.t

type t = {
  t : QV.t;
  clients : (identifier, client) Hashtbl.t;
  mutable counter : int;
}

let disconnect t =
  QV.disconnect t.t

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
  Flow.close flow return_code >|= function
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
        send t.t ~ty:`Connection_terminated reply >|= function
        | `Ok () | `Eof -> ()
      )
  )

let start_connection params clients =
  let domid = Formats.Qrexec.get_exec_params_connect_domain params in
  let port = Formats.Qrexec.get_exec_params_connect_port params in
  let request_id = Cstruct.to_string @@ Cstruct.shift params sizeof_exec_params in
  Log.debug (fun f -> f "service_connect message received: domain %lu, port %lu, request_id %S" domid port request_id);
  Log.debug (fun f -> f "Connecting...");
  match Vchan.Port.of_string (Int32.to_string port) with
  (* XXX: When does this ever happen? *)
  | `Error msg ->
    begin match Hashtbl.find_opt clients request_id with
      | Some client ->
        Hashtbl.remove clients request_id;
        client (`Error msg)
      | None ->
        Log.debug (fun f -> f "request_id %S without client" request_id);
        Lwt.return_unit
    end
  | `Ok port ->
    QV.server ~domid:(Int32.to_int domid) ~port () >>= fun remote ->
    send_hello remote >>= fun () ->
    recv_hello remote >>= fun version ->
    Log.debug (fun f -> f "server connected on port %s, using protocol vers
ion %ld" (Vchan.Port.to_string port) version);
    match Hashtbl.find_opt clients request_id with
    | Some client ->
      Hashtbl.remove clients request_id;
      client (`Ok (Client_flow.create remote))
    | None ->
        Log.debug (fun f -> f "request_id %S without client" request_id);
        Lwt.return_unit

let listen t handler =
  let rec loop () =
    recv t.t >>= function
    | `Ok (`Just_exec | `Exec_cmdline as ty, data) ->
        exec t ~ty ~handler data; loop ()
    | `Ok (`Service_refused, data) ->
      let request_id = Cstruct.to_string data in
      Log.debug (fun f -> f "Service refused for %S" request_id);
      begin match Hashtbl.find_opt t.clients request_id with
        | Some client ->
          Hashtbl.remove t.clients request_id;
          Lwt.async (fun () -> client `Permission_denied);
          loop ()
        | None ->
          Log.warn (fun f -> f "No client for request id %S" request_id);
          loop ()
      end
    | `Ok (`Service_connect, data) ->
      Lwt.async (fun () -> start_connection data t.clients);
      loop ()
    | `Ok (ty, _) ->
        Log.info (fun f -> f "unhandled qrexec message type received: %lu (%s)"
          (int_of_type ty) (string_of_type ty));
        loop ()
    | `Eof ->
        Log.info (fun f -> f "dom0 rexec vchan connection closed; ending listen loop");
        (* Clean up client callbacks that will no longer be called *)
        Hashtbl.reset t.clients;
        return `Done in
  loop () >|= fun `Done -> ()

let qrexec t ~vm ~service client =
  let service_len = 64
  and target_domain_len = 32 in
  if String.length service >= service_len ||
     String.length vm >= target_domain_len
  then raise (Invalid_argument "Qubes.RExec.qrexec: vm or service arguments too long");
  (* XXX: This *should* be unique. The counter could overflow, though *)
  let request_id =
    let id = t.counter in
    t.counter <- id + 1;
    (* a '\000' terminated string of length 32 including '\000' *)
    Printf.sprintf "MIRAGE%025u\000" id in
  let trigger_service_params =
    let zero_pad s len =
      String.init len (fun i -> if i < String.length s then s.[i] else '\000')
    in
    let buf = Cstruct.create sizeof_trigger_service_params in
    set_trigger_service_params_service_name (zero_pad service service_len) 0 buf;
    set_trigger_service_params_target_domain (zero_pad vm target_domain_len) 0 buf;
    set_trigger_service_params_request_id request_id 0 buf;
    buf
  in
  Hashtbl.add t.clients request_id client;
  send t.t ~ty:`Trigger_service trigger_service_params >>= function
  | `Eof ->
    (* XXX: Should we handle this differently? *)
    Lwt.async (fun () -> client (`Error "dom0 closed connection"));
    Lwt.return `Closed
  | `Ok () ->
    Lwt.return `Ok

let connect ~domid () =
  Log.info (fun f -> f "waiting for client...");
  QV.server ~domid ~port:vchan_base_port () >>= fun t ->
  let t = { t; clients = Hashtbl.create 4; counter = 0; } in
  send_hello t.t >>= fun () ->
  recv_hello t.t >>= fun version ->
  Log.info (fun f -> f "client connected, using protocol version %ld" version);
  return t
