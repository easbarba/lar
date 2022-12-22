export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01' # colored GCC warnings and errors
# export SYSTEMD_PAGER= # Uncomment this if you don't like systemctl's auto-paging feature:
#export INPUTRC="$XDG_CONFIG_HOME/readline/inputrc" # readline

. "$HOME/.config/env_variables"
. "$HOME/.profile"

gimme_keys() {
    # kill any ssh-agent running, re-enter new one
    # before starting session and reset ssh env vars

    [[ $(pgrep -x ssh-agent) ]] && killall ssh-agent
    eval "$(ssh-agent -s)"
    ssh-add -q "$HOME/.ssh/id_ed25519" </dev/null

    export SSH_AGENT_PID=$(pgrep ssh-agent)
    export SSH_AUTH_SOCK=$(find /tmp/ssh-* -name agent.*)
}

# WAYLAND
if [ "$(tty)" = "/dev/tty3" ]; then
    gimme_keys
    exec sway
fi
