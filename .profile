# * DEFAULT SOFTWARE:
export VISUAL="emacs"
export EDITOR="nano"
export TERMINAL="mate-terminal"
export BROWSER="google-chrome"

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
export XDG_DATA_DIRS='/var/lib/flatpak/exports/share':"$HOME/.local/share/flatpak/exports/share"

# LOCAL FOLDERS
export LOCAL="$HOME/.local"
export LOCAL_BIN="$LOCAL/bin"
export LOCAL_LIB="$LOCAL/lib"
export LOCAL_MAN="$XDG_DATA_HOME/man"
export LOCAL_INFO="$XDG_DATA_HOME/info"
export LOCAL_DOC="$XDG_DATA_HOME/doc"
export LOCAL_FONTS="$XDG_DATA_HOME/fonts"

# * $HOME BIN DIRECTORIES
export PATH="$HOME/bin":$PATH
export PATH="$LOCAL_BIN":$PATH

# ** DOOM EMACS
export DOOM_DIR="$HOME/.config/emacs"
export PATH="$DOOM_DIR/bin":$PATH

# ** JAVA
export PATH="$HOME/.jbang/bin":$PATH
export SDKMAN_DIR="$HOME/.sdkman"

# ** RUBY
export GEM_HOME="$HOME/.local/gem"
export PATH="$GEM_HOME/bin":$PATH

# ** NPM
export NPM_CONFIG_PREFIX="$HOME/.local/npm"
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/config"
export NPM_CONFIG_CACHE="$XDG_CACHE_HOME/npm"
export PATH="$NPM_CONFIG_PREFIX/bin":$PATH

# ** GOLANG
export GOPATH=$HOME/.local/go
export PATH="$GOPATH/bin":$PATH

# ** GUIX
export GUIX_PROFILE="$HOME/.guix-profile"
export GUIX_LOCPATH="$GUIX_PROFILE/lib/locale"
export PATH="$GUIX_PROFILE/bin":$PATH
