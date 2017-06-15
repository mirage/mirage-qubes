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
