#!/usr/bin/env bash

# DESCRIPTION: AutoStart software

APPS=(udiskie nm-applet blueman-applet emacs emacs_reader unclutter clipit st)

not-found() { [[ ! -x $(command -v "$1") ]]; }

running() { [[ $(pgrep -f "$1") ]]; }

run() { "$@" & }

for app in "${APPS[@]}"
do
    not-found "$app" && continue
    running "$app" && continue

    run "$app"
done
