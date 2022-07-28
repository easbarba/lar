# * WINDOW MANAGER

# dwm bar status
while true; do
    xsetroot -name "$($HOME/.local/bin/s-wm-sysinfo)"
    sleep 30s
done &

~/.local/bin/s-wm-autostart
exec "$HOME/.local/bin/dwm"
