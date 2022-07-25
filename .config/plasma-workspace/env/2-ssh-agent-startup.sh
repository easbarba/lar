#!/usr/bin/env bash

[ -n "$SSH_AGENT_PID" ] || eval "$(ssh-agent -s)"

/usr/bin/ssh-add -q </dev/null
