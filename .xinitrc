# * WINDOW MANAGER

. "$HOME/.profile"
"$HOME/.local/bin/s-wm-autostart"

# while true; do
#     # Log stderror to a file
#     "$HOME/.local/bin/dwm" 2>~/.dwm.log
#     # No error logging
#     #dwm >/dev/null 2>&1
# done
exec "$HOME/.local/bin/dwm"
