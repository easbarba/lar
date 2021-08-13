#!/bin/sh

# * WM FIXES
export _JAVA_AWT_WM_NONREPARENTING=1

# * SOURCE PATHS
. "$HOME/.profile"

# * APPS
xmodmap ~/.config/xmodmap

# * WINDOW MANAGER
exec awesome
