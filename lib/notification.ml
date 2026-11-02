(** A Qubes notification proxy client. Replies are ignored.
    Messages are bincode 1 (fixint, little-endian) behind a u32 length; see notification-proxy src/lib.rs. *)

open Lwt.Infix

let src = Logs.Src.create "qubes.notifications" ~doc:"Qubes notifications"
module Log = (val Logs.src_log src : Logs.LOG)

module Wire = struct
  let le32 v =
    let b = Bytes.create 4 in
    Bytes.set_int32_le b 0 (Int32.of_int v);
    Bytes.unsafe_to_string b

  (* Currently only version 1 is supported by Qubes, version is encoded a ((major << 16) | minor) *)
  let negotiate v =
    if String.length v <> 4 then Error "version must be 4 bytes"
    else
      let v = Int32.to_int (String.get_int32_le v 0) land 0xFFFF_FFFF in
      if v lsr 16 <> 1 then
        Error (Fmt.str "unsupported protocol version %d.%d" (v lsr 16) (v land 0xFFFF))
      else Ok (le32 0x0001_0000)

  (* If the string is not UTF8 valid, replace non ASCII chars by '?' *)
  let sanitize s =
    if String.is_valid_utf_8 s then s
    else String.map (fun c -> if Char.code c < 128 && Char.code c > 31 then c else '?') s

  let add_string b s =
    Buffer.add_int64_le b (Int64.of_int (String.length s));
    Buffer.add_string b s

  let encode ~sequence ~summary ~body =
    let b = Buffer.create 64 in
    Buffer.add_int64_le b sequence;
    Buffer.add_int32_le b 0l (* Notification::V1 *);
    Buffer.add_string b "\000\000\000" (* suppress_sound transient resident *);
    Buffer.add_uint8 b 0 (* urgency: None *);
    Buffer.add_int32_le b 0l (* replaces_id *);
    add_string b (sanitize summary);
    add_string b (sanitize body);
    Buffer.add_int64_le b 0L (* actions: [] *);
    Buffer.add_uint8 b 0 (* category: None *);
    Buffer.add_int32_le b (-1l) (* expire_timeout: server default *);
    Buffer.add_uint8 b 0 (* image: None *);
    le32 (Buffer.length b) ^ Buffer.contents b
end

type t = { flow : RExec.Client_flow.t; mutable sequence : int64 }

(* Read exactly 4 bytes; the first chunk may already hold more. *)
let read_version flow =
  let rec loop acc =
    if String.length acc >= 4 then Lwt.return (Ok (String.sub acc 0 4))
    else
      RExec.Client_flow.read flow >>= function
      | `Stdout s -> loop (acc ^ s)
      | `Stderr _ -> loop acc
      | `Eof -> Lwt.return (Error "end of file")
      | `Exit_code c -> Lwt.return (Error (Printf.sprintf "exit code %ld" c))
  in
  loop ""

(* Ignore replies (ids, dismissals) until EOF. *)
let rec drain flow =
  RExec.Client_flow.read flow >>= function
  | `Stdout _ | `Stderr _ -> drain flow
  | `Eof | `Exit_code _ ->
      Log.info (fun f -> f "notification server closed the connection");
      Lwt.return_unit

(* Connects to the qubes.Notification server, check the version, and ignore replies. *)
let connect ?(vm = "") rexec =
  let connected, u = Lwt.wait () in
  let client r =
    Lwt.wakeup_later u r;
    Lwt.return_unit
  in
  RExec.qrexec rexec ~vm ~service:"qubes.Notifications" client >>= function
  | `Closed -> Lwt.return (Error "qrexec control channel closed")
  | `Ok -> (
      connected >>= function
      | `Closed -> Lwt.return (Error "qrexec control channel closed")
      | `Permission_denied -> Lwt.return (Error "denied by the qrexec policy")
      | `Error e -> Lwt.return (Error e)
      | `Ok flow -> (
          read_version flow >>= function
          | Error e -> Lwt.return (Error ("version negotiation: " ^ e))
          | Ok v -> (
              match Wire.negotiate v with
              | Error e -> Lwt.return (Error e)
              | Ok answer -> (
                  RExec.Client_flow.write flow answer >|= function
                  | `Eof -> Error "version negotiation: end of file"
                  | `Ok () ->
                      Lwt.async (fun () -> drain flow);
                      Ok { flow; sequence = 0L }))))

let notify t ?(body = "") summary =
  let msg = Wire.encode ~sequence:t.sequence ~summary ~body in
  t.sequence <- Int64.succ t.sequence;
  RExec.Client_flow.write t.flow msg >|= function
  | `Ok () -> Ok ()
  | `Eof -> Error "end of file"
