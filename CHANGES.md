### 0.6.1 (2019-01-17)

- mirage-qubes-ipv4: compatibility with ipaddr 3.0.0 (#29 @hannesm)
- upgrade opam files to version 2.0

### 0.6 (2018-09-16)

- qrexec message chunking (#21 @reynir)
- more extensive support of Qubes GUI protocol (#17 @cfcs, #20 @reynir)
- Adjust to tcpip 3.5.0 and mirage-protocols-lwt 1.4.0 changes mirage-qubes-ipv4
  Static_ipv4.Make now requires a Random device and a monotonic clock
  connect requires a Mclock.t
  Mirage_protocols_lwt.IPV4 does not define the type alias ethif (#24 @hannesm)

### 0.5 (2017-06-15)

- Split into 2 opam packages: mirage-qubes and mirage-qubes-ipv4
- Build with jbuilder and release with topkg

### 0.4 (2017-01-20)

- Include an ipv4 sublibrary for automatically configuring ipv4 settings from qubesdb.

- Use and provide interfaces compatible with MirageOS version 3.0.0.

### 0.3 (2016-05-14)

- Replace camlp4 with ppx. Cstruct no longer supports camlp4.

- Require a modern vchan in opam metadata.

- Less verbose logging from RExec.

- Disconnect qrexec clients if handshake fails.
