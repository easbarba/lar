# * ENV VARS

. "$HOME/.profile"

# * INITIAL APPS

"$HOME/.local/bin/s-wm-autostart"

# * WINDOW MANAGER

# exec awesome
[[ -x $HOME/.local/bin/dwm ]] && exec "$HOME/.local/bin/dwm"
[[ -x $HOME/.local/bin/sbcl && -x $HOME/.local/bin/stumpwm ]] && exec "$HOME/.local/bin/stumpwm"
