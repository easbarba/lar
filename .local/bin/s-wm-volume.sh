#!/usr/bin/env bash

# Debug Options
set -euo pipefail

STEP=3
STATE="$1"

# TRANSLATE HUMAN-FRIENDLY STATE TO VOLUME MANAGERS COMMAND
case $STATE in
    up)
        STATE='+';;
    down)
        STATE='-';;
    toggle)
        ;;
esac

usage() {
    printf %s "
Usage:
        up:     raise volume
      down:     decrease volume
    toggle:     mute or power on volume
"
    exit 0
}

# VOLUME MANAGERS

_pactl() {
    case $STATE in
        toggle)
            pactl set-sink-mute @DEFAULT_SINK@ toggle;;
        +|-)
            pactl set-sink-volume @DEFAULT_SINK@ "$STATE$STEP%";;
        *) exit 1;;
    esac
}

# RUN
[[ $# -eq 0 ]] && usage

[[ -x $(command -v pactl) ]] && _pactl

