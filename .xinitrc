# * SOURCE ESSENTIAL ENVIRONMENT VARIABLES
. "$HOME/.profile"

# * START INITIAL APPS
"$HOME/.local/bin/s-wm-autostart"

if [ -x $(command -v i3) ]; then
    exec i3
fi
