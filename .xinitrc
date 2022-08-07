# * ENV VARS
. "$HOME/.profile"

# ** SSH
export SSH_AGENT_PID=$(pgrep ssh-agent)
export SSH_AUTH_SOCK=$(find /tmp/ssh-* -name agent.*)

# * INITIAL APPS

"$HOME/.local/bin/s-wm-autostart"

# * WINDOW MANAGER

if [ -x $HOME/.local/bin/sbcl ] && [ -x $HOME/.local/bin/stumpwm ]; then
    exec "$HOME/.local/bin/stumpwm"
fi

if [ -x $HOME/.local/bin/dwm ]; then
    exec "$HOME/.local/bin/dwm"
fi

if [ -x $(command -v awesome) ]; then
    exec awesome
fi
