. "$HOME/.profile"

[ -x "$(command -v stumpwm)" ] && exec stumpwm
[ -x "$(command -v awesome)" ] && exec awesome
