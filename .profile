# * Description: System Global Variables

# * DEFAULT SOFTWARE:

export VISUAL="emacs"
export EDITOR="nano"
export TERMINAL="alacritty"
export BROWSER="firefox"

# * DIRECTORIES ENV VARS

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_DIRS="/etc/xdg"
export XDG_DESKTOP_DIR="$HOME/Desktop"
export XDG_DOCUMENTS_DIR="$HOME/Documents"
export XDG_DOWNLOAD_DIR="$HOME/Downloads"
export XDG_MUSIC_DIR="$HOME/Musica"
export XDG_PICTURES_DIR="$HOME/Pictures"
export XDG_PUBLICSHARE_DIR="$HOME/Public"
export XDG_TEMPLATES_DIR="$HOME/Templates"
export XDG_VIDEOS_DIR="$HOME/Videos"
[ ! -w "${XDG_RUNTIME_DIR=/run/user/${UID}}" ] && export XDG_RUNTIME_DIR=/tmp

export LOCAL="$HOME/.local"
export LOCAL_BIN="$LOCAL/bin"
export LOCAL_LIB="$LOCAL/lib"
export LOCAL_MAN="$XDG_DATA_HOME/man"
export LOCAL_INFO="$XDG_DATA_HOME/info"
export LOCAL_DOC="$XDG_DATA_HOME/doc"
export LOCAL_FONTS="$XDG_DATA_HOME/fonts"

# $PATH DIRECTORIES

home_bin() {
    export PATH="$HOME/bin"${PATH:+:}${PATH}
    export PATH="$LOCAL_BIN"${PATH:+:}${PATH}
    export INFOPATH="$LOCAL_INFO${INFOPATH:+:}${INFOPATH}"
}

# * PACKAGERS

e_doom() {
    DOOM_DIR="$HOME/.config/emacs"
    PATH="$DOOM_DIR/bin"${PATH:+:}$PATH
}

e_npm() {
    export NPM_CONFIG_PREFIX="$HOME/.local/npm"
    export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/config"
    export NPM_CONFIG_CACHE="$XDG_CACHE_HOME/npm"
    export NPM_CONFIG_TMP="$XDG_RUNTIME_DIR/npm"
    export NPM_HOME="$NPM_CONFIG_PREFIX"
    export NPM_BIN="$NPM_HOME/bin"
    PATH="$NPM_BIN":$PATH
}

e_ruby() {
    # irb
    export irbrc="$XDG_CONFIG_HOME/irb"

    # gem
    export GEM_HOME="$HOME/.local/gem"
    export PATH="$GEM_HOME/bin"${PATH:+:}$PATH
}

e_golang() {
    export GOPATH=$HOME/.local/go
    export PATH="$GOPATH/bin"${PATH:+:}$PATH
}

p_guix() {
    export GUIX_PROFILE="$HOME/.guix-profile"

    # LOCALE
    export GUIX_LOCPATH="$GUIX_PROFILE/lib/locale"

    # RUBY
    local GUIX_GEM_PATH="$GUIX_PROFILE/lib/ruby/vendor_ruby"
    export GEM_PATH="$GUIX_GEM_PATH"${GEM_PATH:+:}$GEM_PATH

    # GUILE
    export GUILE_LOAD_PATH="$GUIX_PROFILE/share/guile/site/3.0"
    export GUILE_LOAD_COMPILED_PATH="$GUIX_PROFILE/lib/guile/3.0/site-ccache:$GUIX_PROFILE/share/guile/site/3.0"

    #    export SSL_CERT_FILE="$GUIX_PROFILE/etc/ssl/certs/ca-certificates.crt"

    # Shared Libraries
    export LD_LIBRARY_PATH="$GUIX_PROFILE/lib"

    # PATH
    export PATH="$GUIX_PROFILE/bin"${PATH:+:}$PATH
}

# * RUN

home_bin

e_doom

e_golang
e_npm
e_ruby
p_guix

# * LOCALE

# export LANG="C.UTF-8"
# export LC_ALL="C.UTF-8"

# export LC_ALL="en_US.utf-8"
# export LANG="en_US.utf-8"
