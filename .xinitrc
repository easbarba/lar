# * ENV VARS

. "$HOME/.profile"

# * INITIAL APPS

"$HOME/.local/bin/s-wm-autostart"

# * WINDOW MANAGER

[[ -x $HOME/.local/bin/sbcl && -x $HOME/.local/bin/stumpwm ]] && exec "$HOME/.local/bin/stumpwm"
[[ -x $HOME/.local/bin/dwm ]] && exec "$HOME/.local/bin/dwm"
[[ -x $(command -v awesome) ]] && exec awesome
