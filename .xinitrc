#!/usr/bin/env bash

# * -- ENVIRONMENT

if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
	   [ -x "$f" ] && . "$f"
    done
    unset f
fi

# * -- UTILITIES

[[ -x $(command -v xhost) ]] && xhost +SI:localuser:"$USER"
[[ -x $(command -v xset) ]] && xset r rate 200 50
[[ -x $(command -v xsetroot) ]] && xsetroot -cursor_name left_ptr
[[ -x $(command -v ssh-agent) ]] && eval "$(ssh-agent)"

# * -- WINDOW MANAGERS

run_window_manager(){
    [ -x "$(command -v dbus-launch)" ] && local bus="dbus-launch"
    local args_extra="--sh-syntax --exit-with-session"
    local window_managers=(awesome emacs dwm xmonad)

    for wm in "${window_managers[@]}"; do
	   if [ -x "$(command -v "$wm")" ]; then
	       local window_manager="$wm"
	       break
	   fi
    done

    if [ "$window_manager" == 0 ]; then
	   echo "No Window Manager known is avaialable!"
	   exit 0
    fi

    export WINDOW_MANAGER=$window_manager # needed to execute some services
    $bus $args_extra $window_manager 2> ~/.local/share/xinit_errors
}

# * -- RUN
run_window_manager
