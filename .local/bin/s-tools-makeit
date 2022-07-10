#!/usr/bin/env bash

# DEBUG OPTIONS
set -euo pipefail

# Description: make folders pointed by command followed by file
# Example: make_it.sh foo/bar/meh.sh bar/foo/file.py
# Shells supported: GNU Bash
# Inspiration: https://github.com/tanrax/terminal-AdvancedNewFile

# * VARS
ARGUMENTS=("$@")
PATH="${ARGUMENTS[0]}"
[[ "$#" -gt 1 ]] && VERBOSE="${ARGUMENTS[1]}"

# * FUNCTIONS
get_last_char() {
    echo "${PATH: -1}"
}

is_dir() {
    [[ $(get_last_char) == "/" ]]
}

get_filename() {
    echo "${PATH##*/}"
}

get_dir_path() {
    local result
    result=$(is_dir && echo "$PATH" || echo ${PATH%$(get_filename)})

    echo "$result"
}

create_dirs() {
    mkdir -p "$(get_dir_path)"
}

create_file() {
    echo '' >"$PATH"
}

run_verbose() {
    if [[ "$VERBOSE" == "--verbose" ]]; then
        echo <<EOF
Path      --> $PATH
Directory --> $(get_dir_path)
Filename  --> $(get_filename)
EOF
    fi
}

run() {
    run_verbose
    create_dirs
    create_file
}

# * MAIN
run
