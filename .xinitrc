# * SOURCE ESSENTIAL ENVIRONMENT VARIABLES
. "$HOME/.profile"

# ** SSH KEYS
export SSH_AGENT_PID=$(pgrep ssh-agent)
export SSH_AUTH_SOCK=$(find /tmp/ssh-* -name agent.*)

# * START INITIAL APPS
"$HOME/.local/bin/s-wm-autostart"

if [[ -x $(command -v i3) ]]; then
    exec i3
fi
