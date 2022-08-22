# * SOURCE INITIAL ENVIRONMENT VARIABLES
. "$HOME/.profile"

if [ -x "$(command -v bspwm)" ]; then
    exec bspwm
fi
