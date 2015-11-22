mirage-qubes
============

An **experimental** unikernel that can run as a QubesOS VM. It acts as a qrexec agent, receiving commands sent from dom0.
You can use this with the [test-mirage][] scripts to deploy the unikernel from your development AppVM. e.g.

    $ opam install mirage
    $ mirage configure --xen
    $ make
    $ test-mirage mir-qubes-test.xen
    Waiting for 'Ready'... OK
    Uploading 'mir-qubes-test.xen' (4187256 bytes)
    Waiting for 'Booting'... OK
    --> Creating volatile image: /var/lib/qubes/appvms/mirage-test/volatile.img...
    --> Loading the VM (type = AppVM)...
    --> Starting Qubes DB...
    --> Setting Qubes DB info for the VM...
    --> Updating firewall rules...
    --> Starting the VM...
    --> Starting the qrexec daemon...
    Waiting for VM's qrexec agent.connected
    MirageOS booting...
    Initialising timer interface
    Initialising console ... done.
    info: Starting qrexec agent; waiting for client...
    info: Got connection
    info: Handshake done; client version is 2

You can invoke commands from dom0. e.g.

    [tal@dom0 bin]$ qvm-run -p --nogui mirage-test echo
    Hi user! Please enter a string:
    Hello
    You wrote "Hello". Bye.

and

    [tal@dom0 bin]$ qvm-run -p --nogui -u root mirage-test quit


LICENSE
-------

Copyright (c) 2015, Thomas Leonard
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
gg

[test-mirage]: https://github.com/talex5/qubes-test-mirage
