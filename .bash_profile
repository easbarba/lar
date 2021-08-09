#!/usr/bin/env bash

set -euo pipefail

# export SWAYSOCK=/run/user/$(id -u)/sway-ipc.$(id -u).$(pgrep -x sway).sock

# If running from tty1 start sway
if [ "$(tty)" = "/dev/tty1" ]; then
	. ~/.profile

	exec sway
fi
