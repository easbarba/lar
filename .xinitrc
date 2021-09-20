#!/bin/sh

# * WM FIXES
export _JAVA_AWT_WM_NONREPARENTING=1

# * SOURCE PATHS
. "$HOME/.profile"

# * APPS
xmodmap ~/.config/xmodmap

# * WINDOW MANAGER

# Statusbar loop
# while true; do
#     xsetroot -name "$(e_sysinfo)"
#     sleep 15s # Update time every minute
# done &

# ~/.config/autostart-scripts/apps
exec awesome # dwm
