# * WINDOW MANAGER

# dwm bar status
while true; do
    xsetroot -name "$($HOME/.local/bin/s-wm-sysinfo)"
    sleep 30s
done &

~/.local/bin/s-wm-autostart

while true; do
    # Log stderror to a file
    "$HOME/.local/bin/dwm" 2>~/.dwm.log
    # No error logging
    #dwm >/dev/null 2>&1
done
# exec "$HOME/.local/bin/dwm"
