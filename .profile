#!/usr/bin/env bash

# * Description: System Global Variables

# * DEFAULT SOFTWARE:
export VISUAL="emacs"
export EDITOR="nano"
export TERMINAL="alacritty"
export BROWSER="firefox"

# * XDG HOMES
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
[ ! -w "${XDG_RUNTIME_DIR=/run/user/${UID}}" ] && export XDG_RUNTIME_DIR=/tmp

# XDG DIRS
export XDG_CONFIG_DIRS="/etc/xdg"
export XDG_DESKTOP_DIR="$HOME/Desktop"
export XDG_DOCUMENTS_DIR="$HOME/Documents"
export XDG_DOWNLOAD_DIR="$HOME/Downloads"
export XDG_MUSIC_DIR="$HOME/Musica"
export XDG_PICTURES_DIR="$HOME/Pictures"
export XDG_PUBLICSHARE_DIR="$HOME/Public"
export XDG_TEMPLATES_DIR="$HOME/Templates"
export XDG_VIDEOS_DIR="$HOME/Videos"

# LOCAL FOLDERS
export LOCAL="$HOME/.local"
export LOCAL_BIN="$LOCAL/bin"
export LOCAL_LIB="$LOCAL/lib"
export LOCAL_MAN="$XDG_DATA_HOME/man"
export LOCAL_INFO="$XDG_DATA_HOME/info"
export LOCAL_DOC="$XDG_DATA_HOME/doc"
export LOCAL_FONTS="$XDG_DATA_HOME/fonts"

# * $HOME BIN DIRECTORIES
export PATH="$HOME/bin"${PATH:+:}${PATH}
export PATH="$LOCAL_BIN"${PATH:+:}${PATH}
# export INFOPATH="$LOCAL_INFO${INFOPATH:+:}${INFOPATH}"

# * MISCELLANEOUS

# ** SECRETS
#export $(cat "$XDG_CONFIG_HOME/secrets" | xargs)
# ===================================================

# ** DOOM Emacs
export DOOM_DIR="$HOME/.config/emacs"
export PATH="$DOOM_DIR/bin":$PATH
# ===================================================

# * LANGUAGES PACKAGE MANAGERS

# COMMON LISP
export PATH="${HOME}/.local/bin${PATH:+:}$PATH"

# ** RUBY
export GEM_HOME="$HOME/.local/gem"
export PATH="$GEM_HOME/bin"${PATH:+:}$PATH
# ===================================================

# ** GOLANG
export GOPATH=$HOME/.local/go
export PATH="$GOPATH/bin"${PATH:+:}$PATH
# ===================================================

# ** NPM
export NPM_CONFIG_PREFIX="$HOME/.local/npm"
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/config"
export NPM_CONFIG_CACHE="$XDG_CACHE_HOME/npm"
export PATH="$NPM_CONFIG_PREFIX/bin"${PATH:+:}$PATH
# ===================================================

# * FOREIGN SYSTEM PACKAGE MANAGER

# ** GUIX
export GUIX_PROFILE="$HOME/.guix-profile"
export GUIX_LOCPATH="$GUIX_PROFILE/lib/locale"
export GUILE_LOAD_PATH="$GUIX_PROFILE/share/guile/site/3.0"
export GUILE_LOAD_COMPILED_PATH="$GUIX_PROFILE/lib/guile/3.0/site-ccache:$GUIX_PROFILE/share/guile/site/3.0"
export PATH="$GUIX_PROFILE/bin"${PATH:+:}$PATH
# ===================================================

# ** NIX
export NIX_PROFILE=$HOME/.nix-profile
export PATH="$NIX_PROFILE/bin"${PATH:+:}$PATH
# ===================================================

# * WINDOW MANAGER
export _JAVA_AWT_WM_NONREPARENTING=1

if [ -n WAYLAND_DISPLAY ]; then
    export SSH_AUTH_SOCK=/run/user/1000/ssh-agent.socket
fi
