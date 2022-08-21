# * SOURCE ESSENTIAL ENVIRONMENT VARIABLES
. "$HOME/.profile"

# * START INITIAL APPS
"$HOME/.local/bin/s-wm-autostart"

if [ -x "$(command -v bspwm)" ]; then
    exec bspwm
fi
