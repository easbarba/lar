#!/usr/bin/env bash

# DEBUG
#set -euo pipefail

# * TTY1
if [ -z "${DISPLAY}" ] && [ "${XDG_VTNR}" -eq 1 ]; then
    mkdir -pv ~/.local/share/xinit
    exec startx 2>>~/.local/share/xinit/errors
fi

# * TTY3
if [ "$(tty)" = "/dev/tty3" ]; then
    export SWAYSOCK=/run/user/$(id -u)/sway-ipc.$(id -u).$(pgrep -x sway).sock
    exec sway
fi
