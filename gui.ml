open Lwt.Infix

let gui_agent_port =
  match Vchan.Port.of_string "6000" with
  | `Error msg -> failwith msg
  | `Ok port -> port

let connect ~domid () =
  Log.info "gui-agent: waiting for client...";
  Vchan_xen.server ~domid ~port:gui_agent_port () >|= fun gui ->
  Log.info "gui-agent: client connected";
  gui
