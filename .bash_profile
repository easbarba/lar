. $HOME/.profile

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# export SYSTEMD_PAGER= # Uncomment this if you don't like systemctl's auto-paging feature:

# User specific aliases and functions
if [ -d ~/.bashrc.d ]; then
    for rc in ~/.bashrc.d/*; do
        if [ -f "$rc" ]; then
            . "$rc"
        fi
    done
fi

unset rc

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]; then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH

# If running from tty1 start sway
if [ "$(tty)" = "/dev/tty1" ]; then
    [[ $(pgrep -x ssh-agent) ]] && killall ssh-agent
    s-tools-ssh

    exec startx
fi

# If running from tty1 start sway
if [ "$(tty)" = "/dev/tty2" ]; then
    [[ $(pgrep -x ssh-agent) ]] && killall ssh-agent
    s-tools-ssh

    export SSH_AUTH_SOCK=/run/user/1000/ssh-agent.socket

    exec sway
fi

# ** readline
#export INPUTRC="$XDG_CONFIG_HOME/readline/inputrc"
