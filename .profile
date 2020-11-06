#!/usr/bin/env bash

# * Description: System Global Variables
# * DEFAULT SOFTWARE:

export EDITOR="emacs -nw"
export TERMINAL="st"
export BROWSER="google-chrome"
export EXPLORER="emacs -Q -f dired"
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# * FOLDERS
# ** XDG FOLDERS:

export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_CONFIG_DIRS="/etc/xdg"
export XDG_DESKTOP_DIR="${HOME}/Desktop"
export XDG_DOCUMENTS_DIR="${HOME}/Documents"
export XDG_DOWNLOAD_DIR="${HOME}/Downloads"
export XDG_MUSIC_DIR="${HOME}/Musica"
export XDG_PICTURES_DIR="${HOME}/Pictures"
export XDG_PUBLICSHARE_DIR="${HOME}/Public"
export XDG_TEMPLATES_DIR="${HOME}/Templates"
export XDG_VIDEOS_DIR="${HOME}/Videos"
[ ! -w "${XDG_RUNTIME_DIR=/run/user/${UID}}" ] && export XDG_RUNTIME_DIR=/tmp

# ** LOCAL FOLDERS

export LOCAL="${HOME}/.local"
export LOCAL_BIN="${LOCAL}/bin"
export LOCAL_LIB="${LOCAL}/lib"
export LOCAL_MAN="${XDG_DATA_HOME}/man"
export LOCAL_INFO="${XDG_DATA_HOME}/info"
export LOCAL_DOC="${XDG_DATA_HOME}/doc"
export LOCAL_FONTS="${XDG_DATA_HOME}/fonts"

# * INFO

export INFOPATH="${LOCAL_INFO}${INFOPATH:+:}${INFOPATH}"

export PATH="${HOME}/bin"${PATH:+:}${PATH}
export PATH="$LOCAL_BIN"${PATH:+:}${PATH}

# * RUN

[[ -f "$XDG_CONFIG_HOME/bash/shell-paths" ]] && source "$XDG_CONFIG_HOME/bash/shell-paths"
