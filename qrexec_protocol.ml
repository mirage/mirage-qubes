cstruct msg_header {
  uint32_t ty;
  uint32_t len;
} as little_endian

cstruct peer_info {
  uint32_t version;
} as little_endian

cstruct exec_params {
  uint32_t connect_domain;
  uint32_t connect_port;
  (* rest of message is command line *)
} as little_endian

cstruct exit_status {
  (* XXX: size of message depends on native int size?? *)
  uint64_t return_code;
} as little_endian

type msg_type =
  [ `Exec_cmdline
  | `Just_exec
  | `Service_connect
  | `Trigger_service
  | `Connection_terminated
  | `Hello
  | `Data_stdin
  | `Data_stdout
  | `Data_stderr
  | `Data_exit_code ]

let type_of_int = function
  | 0x190l -> `Data_stdin
  | 0x191l -> `Data_stdout
  | 0x192l -> `Data_stderr
  | 0x193l -> `Data_exit_code
  | 0x200l -> `Exec_cmdline
  | 0x201l -> `Just_exec
  | 0x202l -> `Service_connect
  | 0x210l -> `Trigger_service
  | 0x211l -> `Connection_terminated
  | 0x300l -> `Hello
  | x -> `Unknown x

let int_of_type = function
  | `Data_stdin -> 0x190l
  | `Data_stdout -> 0x191l
  | `Data_stderr -> 0x192l
  | `Data_exit_code -> 0x193l
  | `Exec_cmdline -> 0x200l
  | `Just_exec -> 0x201l
  | `Service_connect -> 0x202l
  | `Trigger_service -> 0x210l
  | `Connection_terminated -> 0x211l
  | `Hello -> 0x300l
  | `Unknown x -> x
