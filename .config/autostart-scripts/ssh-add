#!/usr/bin/env bash

if [ "$DESKTOP_SESSION" == "plasma" ]; then
    export SSH_ASKPASS=/usr/bin/ksshaskpass
    ssh-add "$HOME/.ssh/id_ed25519"  < /dev/null
fi
