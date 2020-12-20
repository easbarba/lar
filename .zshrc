#!/usr/bin/env zsh

# * Zsh
# Descricao: Zsh - Configuracoes

# * ALIASES

alias e-reload="source $HOME/.zshrc"

# * DEFAULT

e-default() {
    # Command completion
    autoload -Uz compinit promptinit
    compinit
    promptinit

    # default prompt theme
    prompt clint

    # emacs binds
    bindkey -e

    # History search
    autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
    zle -N up-line-or-beginning-search
    zle -N down-line-or-beginning-search
    [[ -n "${key[Up]}"   ]] && bindkey -- "${key[Up]}"   up-line-or-beginning-search
    [[ -n "${key[Down]}" ]] && bindkey -- "${key[Down]}" down-line-or-beginning-search
}

# * ZPLUG

e-zplug-install-plugins() {
    zplug "zplug/zplug", hook-build:"zplug --self-manage"

    # prezto
    zplug "modules/tmux",       from:prezto
    zplug "modules/history",    from:prezto
    zplug "modules/utility",    from:prezto
    zplug "modules/terminal",   from:prezto
    zplug "modules/editor",     from:prezto
    zplug "modules/directory",  from:prezto
    zplug "modules/completion", from:prezto

    # zsh users
    zplug "zsh-users/zsh-completions",              defer:0
    zplug "zsh-users/zsh-autosuggestions",          defer:2, on:"zsh-users/zsh-completions"
    zplug "zsh-users/zsh-syntax-highlighting",      defer:3, on:"zsh-users/zsh-autosuggestions"
    zplug "zsh-users/zsh-history-substring-search", defer:3, on:"zsh-users/zsh-syntax-highlighting"

    # Plugins from oh my zsh
    zplug "plugins/git", from:oh-my-zsh

    # misc
    zplug "b4b4r07/enhancd", use:init.sh
    zplug "jocelynmallon/zshmarks" # Bookmarks and jump
    zplug "supercrabtree/k" # Enhanced dir list with git features
    zplug "djui/alias-tips" # Tips for aliases
    zplug "felixr/docker-zsh-completion" # Docker completion
    zplug "denysdovhan/spaceship-prompt", use:spaceship.zsh, from:github, as:theme

    export ZSH_PLUGINS_ALIAS_TIPS_TEXT=' alias hint: '
    export KEYTIMEOUT=1

    if ! zplug check; then # install plugins
	zplug install
    fi
}

e-zplug-bootstrap() {
    local zplug_dir="${HOME}/.config/zplug"

    git clone https://github.com/zplug/zplug "$zplug_dir"

    source "$zplug_dir/init.zsh"

    e-zplug-install-plugins
}


e-zplug-activate()
{
    local zplug_dir="${HOME}/.config/zplug"

    source "$zplug_dir/init.zsh"

    zplug update
    zplug load
}


e-zplug()
{
    [[ ! -f $HOME/.config/zplug/init.zsh ]] && e-zplug-bootstrap

    e-zplug-activate
}

# * BEGIN
e-zplug || e-default

# * SOURCING
export SHELL_ADDS="$HOME/bin/shell-additionals"; [ -f "$SHELL_ADDS" ] && source "$SHELL_ADDS"
[ -x "$(command -v starship)" ] && eval "$(starship init zsh)"
export ASDF_BIN="$HOME/.config/asdf/completions/asdf.zsh"; [[ -f $ASDF_BIN ]] && source "$ASDF_BIN"
