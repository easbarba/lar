#!/usr/bin/env bash

[[ -z "$SSH_AUTH_SOCK" ]] || eval "$(ssh-agent -s)"
