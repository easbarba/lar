# import environment.d vars to GNU Bash
export $(/usr/lib/systemd/user-environment-generators/30-systemd-environment-d-generator | xargs) # do not quote this!

[[ -f $HOME/.profile ]] && . "$HOME/.profile"

setup_keys() {
    # kill any ssh-agent running, re-enter new one
    # before starting session and reset ssh env vars

    [[ $(pgrep -x ssh-agent) ]] && killall ssh-agent
    eval "$(ssh-agent -s)"
    ssh-add -q "$HOME/.ssh/id_ed25519" </dev/null

    export SSH_AGENT_PID=$(pgrep ssh-agent)
    export SSH_AUTH_SOCK=$(find /tmp/ssh-* -name agent.*)
}

wm() {
    # Check if dbus is present
    [[ -x $(command -v dwl) ]] && exec dwl -s somebar #>"$HOME/.cache/dwltags"
    # exec sway
}

# WAYLAND
if [ "$(tty)" = "/dev/tty3" ]; then
    setup_keys
    wm
fi
