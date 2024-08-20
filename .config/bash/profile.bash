sshkeys() {
    # kill any ssh-agent running, re-enter new one
    # before starting session and reset ssh env vars
    killall ssh-agent

    # if [ -z "$SSH_AGENT_PID" ]; then
    eval "$(ssh-agent -s)"
    # fi

    ssh-add -q "$HOME/.ssh/id_ed25519" </dev/null

    export SSH_AGENT_PID=$(pgrep ssh-agent)
    export SSH_AUTH_SOCK=$(find /tmp/ssh-* -name agent.*)
}

wm() {
#    exec /usr/lib/plasma-dbus-run-session-if-needed
#    /usr/bin/startplasma-wayland

    #    [[ -x $(command -v wayfire) ]] && exec wayfire
#    [[ -x $(command -v miracle) ]] && exec miracle
    [[ -x $(command -v sway) ]] && exec dbus-run-session sway
    [[ -x $(command -v river) ]] && exec river
}

# WAYLAND
if [ -z "$WAYLAND_DISPLAY" ] && [ "$XDG_VTNR" -eq 3 ]; then # [ "$(tty)" = "/dev/tty3" ]
    sshkeys
    wm
fi
