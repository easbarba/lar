#!/usr/bin/env bash

# DESCRIPTION: AutoStart software


services()
{
    eval "$(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)"
    export GNOME_KEYRING_CONTROL GNOME_KEYRING_PID GPG_AGENT_INFO SSH_AUTH_SOCK
}

APPS=(udiskie nm-applet blueman-applet emacs unclutter diodon mate-terminal atril mate-power-manager)

not-found() { [[ ! -x $(command -v "$1") ]]; }

running() { [[ $(pgrep -f "$1") ]]; }

run() { "$@" & }

autostart()
{
    for app in "${APPS[@]}"
    do
	not-found "$app" && continue
	running "$app" && continue

	run "$app"
    done
}

services
autostart
