. "$HOME/.profile"

[ -x "$(command -v mate-session)" ] && exec mate-session
[ -x "$(command -v awesome)" ] && exec awesome
