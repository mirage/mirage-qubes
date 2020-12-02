### v0.9.1 (2020-12-02)

- RExec: being future compatible, and negotiate protocol version 2 (#56 #57 @reynir)
- Remove Utils module (#58 @hannesm)
- RExec: close connection with peer on unknown messages, instead of raising an exception (#61 @reynir)

### v0.9.0 (2020-10-22)

- adapt to mirage-xen 6.0.0 and vchan-xen 6.0.0, only metadata changes (#55 @hannesm)

### v0.8.3 (2020-06-24)

- adapt to tcpip 5.0.0 API changes (#53 @hannesm)

### v0.8.2 (2020-05-13)

- provide Qubes.DB.got_new_commit to wait for specific commit writes in QubesDB
  (#52 @linse @hannesm)

### v0.8.1 (2020-03-13)

- fix compile error with cstruct 5.1 (#48 @talex5, reported by @hannesm)
- now using dune, no need for `pkg/pkg.ml` anymore (#50 @hannesm)

### v0.8.0 (2019-11-02)

- adapt to mirage-protocols tcpip 4.0.0; mirage-xen vchan-xen 5.0.0 (#46 @hannesm)
- support initiating qrexec calls (#39 @reynir @yomimono @linse, review by @cfcs, discussion in #35 #36)
- add trigger_service_params and service_refused format string_of_type for messages (#43 @yomimono @linse)
- GUI window support (#32 #33 #37 #41 @cfcs @reynir @yomimono)

### v0.7.0 (2019-03-06)

- mirage-qubes-ipv4: compatibility with mirage-protocols 2.0.0 and mirage-net 2.0.0
  (#31 @yomimono)

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
