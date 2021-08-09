#!/bin/sh

# * WM FIXES
export _JAVA_AWT_WM_NONREPARENTING=1

# * SOURCE PATHS
. "$HOME/.xprofile"

# * APPS
# xmodmap ~/.config/xmodmap

# * WINDOW MANAGER
exec sway
