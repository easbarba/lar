setup_keys() {
    # kill any ssh-agent running, re-enter new one
    # before starting session and reset ssh env vars
    killall ssh-agent

    eval "$(ssh-agent -s)"
    ssh-add -q "$HOME/.ssh/id_ed25519" </dev/null

    export SSH_AGENT_PID=$(pgrep ssh-agent)
    export SSH_AUTH_SOCK=$(find /tmp/ssh-* -name agent.*)
}

wm() {
    [[ -x $(command -v sway) ]] &&   exec sway
}

# WAYLAND
if [ "$(tty)" = "/dev/tty3" ]; then
    setup_keys
    wm
fi
