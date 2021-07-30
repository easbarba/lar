#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

[[ -f $sysresources ]] && xrdb -merge $sysresources
[[ -f $sysmodmap ]] && xmodmap $sysmodmap
[[ -f "$userresources" ]] && xrdb -merge "$userresources"
[[ -f "$usermodmap" ]] && xmodmap "$usermodmap"

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

# --------------------------------------------------------

# * WM FIXES
export _JAVA_AWT_WM_NONREPARENTING=1

# * SOURCE PATHS
. "$HOME/.xprofile"

# * STATUSBAR
while true; do
    xsetroot -name "$(e_sysinfo)"
    sleep 20s # Update time every minute
done &

# * EXEC

"$HOME"/.config/autostart-scripts/apps

while true; do
    # Log stderror to a file
    dwm 2>~/.local/share/dwm.log
    # No error logging
    #dwm >/dev/null 2>&1
done
