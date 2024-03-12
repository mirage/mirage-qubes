(* Copyright (C) 2015, Thomas Leonard
   See the README file for details. *)

open Lwt.Infix
open Formats.Qrexec

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
  | `Ok y -> Lwt.return y
  | `Error (`Unknown msg) -> Lwt.fail_with msg
  | `Eof -> Lwt.fail End_of_file

let vchan_base_port =
  match Vchan.Port.of_string "512" with
  | Error (`Msg msg) -> failwith msg
  | Ok port -> port


let max_data_chunk_v2 = 4096
(** Max size for data chunks. See MAX_DATA_CHUNK in qubes-linux-utils/qrexec-lib/qrexec.h *)

let max_data_chunk_v3 = 65536
(** protocol version 3+ *)

let max_data_chunk : Formats.Qrexec.version -> int = function
  | `V2 -> max_data_chunk_v2
  | `V3 -> max_data_chunk_v3

let rec send t ~version ~ty data =
  let ldata = Bytes.length data in
  let lmin = min (max_data_chunk version) ldata in
  let hdr = Bytes.create sizeof_msg_header in
  set_msg_header_ty hdr (int_of_type ty);
  set_msg_header_len hdr (ldata |> Int32.of_int);
  if ldata = lmin
  then QV.send t [hdr; data]
  else
    let data' = Bytes.sub data lmin (ldata-lmin) in
    let data = Bytes.sub data 0 lmin in
    QV.send t [hdr; data] >>= function
      | `Eof -> Lwt.return `Eof
      | `Ok () ->
        send t ~version ~ty data'

let recv t =
  QV.recv t >>!= fun (hdr, data) ->
  let ty = get_msg_header_ty hdr |> type_of_int in
  Lwt.return (`Ok (ty, data))

module Flow = struct
  type t = {
    dstream : QV.t;
    mutable stdin_buf : Bytes.t;
    ty : [`Just_exec | `Exec_cmdline];
    version : Formats.Qrexec.version;
  }

  let create ~version ~ty dstream =
    {dstream; stdin_buf = Bytes.empty; ty; version}

  let push ~stream flow buf =
    match flow.ty with
    | `Just_exec -> Lwt.return_unit
    | `Exec_cmdline ->
    if Bytes.length buf > 0 then
      send flow.dstream ~version:flow.version ~ty:stream buf >>= or_fail
    else
      Lwt.return_unit

  let pushf ~stream flow fmt =
    fmt |> Printf.ksprintf @@ fun s ->
      push ~stream flow (Bytes.of_string (s ^ "\n"))

  let write = push ~stream:`Data_stdout
  let ewrite = push ~stream:`Data_stderr

  let writef fmt = pushf ~stream:`Data_stdout fmt
  let ewritef fmt = pushf ~stream:`Data_stderr fmt

  let read_raw flow =
    match flow.ty with
    | `Just_exec -> Lwt.return `Eof
    | `Exec_cmdline ->
    recv flow.dstream >>!= function
    | `Data_stdin, empty when Bytes.length empty = 0 -> Lwt.return `Eof
    | `Data_stdin, data -> Lwt.return (`Ok data)
    | ty, _ ->
      Log.err (fun f -> f "Unknown message type %ld received" (int_of_type ty));
      QV.disconnect flow.dstream >>= fun () ->
      Lwt.return `Eof

  let read flow =
    if Bytes.length flow.stdin_buf > 0 then (
      let retval = flow.stdin_buf in
      flow.stdin_buf <- Bytes.empty;
      Lwt.return (`Ok retval)
    ) else read_raw flow

  let rec read_line flow =
    let buf = Bytes.to_string flow.stdin_buf in
    let i =
      try Some (String.index buf '\n')
      with Not_found -> None in
    match i with
    | Some i ->
        let retval = String.sub buf 0 i in
        Bytes.blit flow.stdin_buf 0 flow.stdin_buf (i + 1) i;
        Lwt.return (`Ok retval)
    | None ->
        read_raw flow >>!= fun buf ->
        flow.stdin_buf <- Bytes.cat flow.stdin_buf buf;
        read_line flow

  let close flow return_code =
    let msg = Bytes.create sizeof_exit_status in
    set_exit_status_return_code msg (Int32.of_int return_code);
    Lwt.finalize
      (fun () ->
        send flow.dstream ~version:flow.version ~ty:`Data_stdout (Bytes.empty) >>!= fun () ->
        send flow.dstream ~version:flow.version ~ty:`Data_exit_code msg >>!= fun () ->
        Lwt.return (`Ok ())
      )
      (fun () -> QV.disconnect flow.dstream)
end

module Client_flow = struct
  type t = {
    dstream : QV.t;
    mutable stdout_buf : Bytes.t;
    mutable stderr_buf : Bytes.t;
    version : Formats.Qrexec.version;
  }

  let create ~version dstream =
    { dstream; stdout_buf = Bytes.empty;
      stderr_buf = Bytes.empty; version }

  let write t data = send ~version:t.version ~ty:`Data_stdin t.dstream data

  let writef t fmt =
    fmt |> Printf.ksprintf @@ fun s ->
    send ~version:t.version ~ty:`Data_stdin t.dstream (Bytes.of_string s)

  let next_msg t =
    recv t.dstream >>= function
    | `Ok (`Data_stdout, data) ->
      t.stdout_buf <- Bytes.cat t.stdout_buf data;
      Lwt.return (`Ok t)
    | `Ok (`Data_stderr, data) ->
      t.stderr_buf <- Bytes.cat t.stderr_buf data;
      Lwt.return (`Ok t)
    | `Ok (`Data_exit_code, data) ->
      Lwt.return (`Exit_code (Formats.Qrexec.get_exit_status_return_code data))
    | `Ok (ty, _) ->
      Log.err Formats.Qrexec.(fun f -> f "unexpected message of type %ld (%s) received; \
                                          closing connection" (int_of_type ty) (string_of_type ty));
      QV.disconnect t.dstream >>= fun () ->
      Lwt.return `Eof
    | `Eof -> Lwt.return `Eof

  let read t =
    let rec aux = function
      | `Eof | `Exit_code _ as s -> Lwt.return s
      | `Ok t ->
        let drain_stdout () =
          let output = t.stdout_buf in
          t.stdout_buf <- Bytes.empty;
          Lwt.return (`Stdout output)
        and drain_stderr () =
          let output = t.stderr_buf in
          t.stderr_buf <- Bytes.empty;
          Lwt.return (`Stderr output)
        in
        if Bytes.length t.stdout_buf > 0
        then drain_stdout ()
        else if Bytes.length t.stderr_buf > 0
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
  version : version;
}

let disconnect t =
  QV.disconnect t.t

type handler = user:string -> string -> Flow.t -> int Lwt.t

let send_hello t =
  let version = `V3 in
  let hello = Bytes.create sizeof_peer_info in
  set_peer_info_version hello (int_of_version version);
  send t ~version ~ty:`Hello hello >>= function
  | `Eof -> Fmt.failwith "End-of-file sending msg_hello"
  | `Ok () -> Lwt.return_unit

let recv_hello t =
  recv t >>= function
  | `Eof -> Fmt.failwith "End-of-file waiting for msg_hello"
  | `Ok (`Hello, resp) ->
    let peer_version = get_peer_info_version resp in
    Lwt.return (version_of_int peer_version)
  | `Ok (ty, _) -> Fmt.failwith "Expected msg_hello, got %ld" (int_of_type ty)

let negotiate_version (peer_version : [ version | `Unknown_version of int32 ])
  : version =
  let version =
    match peer_version with
    | `Unknown_version x -> if x < int_of_version `V2
      then Fmt.failwith "Unsupported qrexec version %lu" x
      else `V3
    | #version as version -> version
  in
  Log.debug (fun f -> f "remote end wants to use protocol version %lu, \
                         continuing with version %lu"
                (int_of_version peer_version) (int_of_version version));
  version



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
          recv_hello client >>= fun peer_version ->
          send_hello client >|= fun () ->
          let version = negotiate_version peer_version in
          Flow.create ~version ~ty client
        )
        (fun ex -> QV.disconnect client >>= fun () -> Lwt.fail ex)
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
      Lwt.return_unit
    )

let port_of_int i =
  match Int32.to_string i |> Vchan.Port.of_string with
  | Ok p -> p
  | Error (`Msg msg) -> failwith msg

let parse_cmdline cmd =
  if cmd.[String.length cmd - 1] <> '\x00' then
    Lwt.fail_with "Command not null-terminated"
  else (
    let cmd = String.sub cmd 0 (String.length cmd - 1) in
    match cmd |> split ':' with
    | None -> Fmt.failwith "Missing ':' in %S" cmd
    | Some (user, cmd) -> Lwt.return (user, cmd)
  )

let exec t ~ty ~handler msg =
  Lwt.async (fun () ->
    let domid = get_exec_params_connect_domain msg |> Int32.to_int in
    let port = get_exec_params_connect_port msg |> port_of_int in
    let len = Bytes.length msg in
    let cmdline = Bytes.sub_string msg sizeof_exec_params (len - sizeof_exec_params) in
    Log.debug (fun f -> f "Execute %S" cmdline);
    Lwt.finalize
      (fun () ->
        with_flow ~ty ~domid ~port (fun flow ->
          parse_cmdline cmdline >>= fun (user, cmd) ->
          handler ~user cmd flow >>= fun return_code ->
          Log.debug (fun f -> f "%S returned exit status %d" cmd return_code);
          Lwt.return return_code
        )
      )
      (fun () ->
        let reply = Bytes.sub msg 0 sizeof_exec_params in
        send t.t ~version:t.version ~ty:`Connection_terminated reply >|= function
        | `Ok () | `Eof -> ()
      )
  )

let start_connection params clients =
  let domid = Formats.Qrexec.get_exec_params_connect_domain params in
  let port = Formats.Qrexec.get_exec_params_connect_port params in
  let request_id = Bytes.to_string @@ Bytes.sub params sizeof_exec_params (Bytes.length params) in
  Log.debug (fun f -> f "service_connect message received: domain %lu, port %lu, request_id %S" domid port request_id);
  Log.debug (fun f -> f "Connecting...");
  match Vchan.Port.of_string (Int32.to_string port) with
  (* XXX: When does this ever happen? *)
  | Error (`Msg msg) ->
    begin match Hashtbl.find_opt clients request_id with
      | Some client ->
        Hashtbl.remove clients request_id;
        client (`Error msg)
      | None ->
        Log.debug (fun f -> f "request_id %S without client" request_id);
        Lwt.return_unit
    end
  | Ok port ->
    QV.server ~domid:(Int32.to_int domid) ~port () >>= fun remote ->
    send_hello remote >>= fun () ->
    recv_hello remote >>= fun peer_version ->
    let version = negotiate_version peer_version in
    Log.debug (fun f -> f "server connected on port %s, using protocol vers
ion %ld" (Vchan.Port.to_string port) (int_of_version version));
    match Hashtbl.find_opt clients request_id with
    | Some client ->
      Hashtbl.remove clients request_id;
      client (`Ok (Client_flow.create ~version remote))
    | None ->
        Log.debug (fun f -> f "request_id %S without client" request_id);
        Lwt.return_unit

let listen t handler =
  let rec loop () =
    recv t.t >>= function
    | `Ok (`Just_exec | `Exec_cmdline as ty, data) ->
        exec t ~ty ~handler data; loop ()
    | `Ok (`Service_refused, data) ->
      let request_id = Bytes.to_string data in
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
        Log.warn (fun f -> f "unhandled qrexec message type received: %lu (%s)"
          (int_of_type ty) (string_of_type ty));
        loop ()
    | `Eof ->
        Log.info (fun f -> f "dom0 rexec vchan connection closed; ending listen loop");
        (* Clean up client callbacks that will no longer be called *)
        Hashtbl.reset t.clients;
        Lwt.return `Done in
  loop () >|= fun `Done -> ()

let service_params ~version ~service ~vm ~request_id =
  let zero_pad s len =
    String.init len (fun i -> if i < String.length s then s.[i] else '\000')
  in
  match version with
  | `V2 ->
    let service_len = 64
    and target_domain_len = 32 in
    if String.length service >= service_len ||
       String.length vm >= target_domain_len
    then raise (Invalid_argument "Qubes.RExec.qrexec: vm or service arguments too long");
    let buf = Bytes.create sizeof_trigger_service_params in
    set_trigger_service_params_service_name (zero_pad service service_len) 0 buf;
    set_trigger_service_params_target_domain (zero_pad vm target_domain_len) 0 buf;
    set_trigger_service_params_request_id request_id 0 buf;
    `Trigger_service, buf
  | `V3 ->
    let target_domain_len = 64 in
    let buf = Bytes.create (sizeof_trigger_service_params3 + String.length request_id) in
    set_trigger_service_params3_target_domain (zero_pad vm target_domain_len) 0 buf;
    set_trigger_service_params3_request_id request_id 0 buf;
    Bytes.blit (Bytes.of_string request_id) 0 buf sizeof_trigger_service_params3 (String.length request_id);
    `Trigger_service3, buf

let qrexec t ~vm ~service client =
  (* XXX: This *should* be unique. The counter could overflow, though *)
  let request_id =
    let id = t.counter in
    t.counter <- id + 1;
    (* a '\000' terminated string of length 32 including '\000' *)
    Printf.sprintf "MIRAGE%025u\000" id in
  let ty, trigger_service_params =
    service_params ~version:t.version ~service ~vm ~request_id in
  Hashtbl.add t.clients request_id client;
  send t.t ~version:t.version ~ty trigger_service_params >>= function
  | `Eof ->
    (* XXX: Should we handle this differently? *)
    Lwt.async (fun () -> client (`Error "dom0 closed connection"));
    Lwt.return `Closed
  | `Ok () ->
    Lwt.return `Ok

let connect ~domid () =
  Log.info (fun f -> f "waiting for client...");
  QV.server ~domid ~port:vchan_base_port () >>= fun t ->
  send_hello t >>= fun () ->
  recv_hello t >>= fun peer_version ->
  let version = negotiate_version peer_version in
  Log.info (fun f -> f "client connected, using protocol version %ld" (int_of_version version));
  Lwt.return { t; clients = Hashtbl.create 4; counter = 0; version; }
