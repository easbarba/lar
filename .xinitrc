# * SOURCE ESSENTIAL ENVIRONMENT VARIABLES
. "$HOME/.profile"

# ** SSH KEYS
export SSH_AGENT_PID=$(pgrep ssh-agent)
export SSH_AUTH_SOCK=$(find /tmp/ssh-* -name agent.*)

# * START INITIAL APPS
"$HOME/.local/bin/s-wm-autostart"

# * WINDOW MANAGERS
if [ -x $HOME/.local/bin/sbcl ] && [ -x $HOME/.local/bin/stumpwm ]; then
    exec "$HOME/.local/bin/stumpwm"
fi

if [[ -x $HOME/.local/bin/dwm ]]; then
    exec "$HOME/.local/bin/dwm"
fi

if [[ -x $(command -v i3) ]]; then
    exec i3
fi
