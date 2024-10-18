#!/usr/bin/env -S guix shell bash coreutils grep sed glibc util-linux:lib pulseaudio libsm libxrender mesa libxext libx11 libsm libice gdb -- bash
LOADER=$GUIX_ENVIRONMENT/lib/ld-linux-x86-64.so.2 LD_LIBRARY_PATH=$GUIX_ENVIRONMENT/lib "`dirname $0`/RunCuisOnLinux.sh" "$@"
