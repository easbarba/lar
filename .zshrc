#!/usr/bin/env zsh

# * Zsh
# Descricao: Zsh - Configuration

# * ALIASES

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

e-zplug-home()
{
    export ZPLUG_HOME="$HOME/.zplug"
    export ZSH_PLUGINS_ALIAS_TIPS_TEXT=' alias hint: '
    export KEYTIMEOUT=1
}

e-zplug-get()
{
    [ -d $ZPLUG_HOME ] && return

    git clone https://github.com/zplug/zplug "$ZPLUG_HOME"
}

e-zplug-activate()
{
    source "$ZPLUG_HOME/init.zsh"
}

e-zplug-install-plugins() {
    zplug "zplug/zplug", hook-build:"zplug --self-manage"

    # prezto
    zplug "modules/tmux",       from:prezto
    # zplug "modules/history",    from:prezto
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

    # nix
    zplug "chisui/zsh-nix-shell"
    zplug "spwhitt/nix-zsh-completions"

    if ! zplug check; then # install plugins
	zplug install
    fi
}

e-zplug-check()
{
    if [ -f "$ZPLUG_HOME/init.zsh" ]; then
	return true
    else
	return false
    fi
}

e-zplug()
{
    e-zplug-home
    e-zplug-get
    e-zplug-activate
    e-zplug-install-plugins

    # foobar
    zplug update
    zplug load

    e-zplug-check
}

# * BEGIN
e-zplug || e-default


# * SOURCING

e_prompt()
{
    local liquid="$HOME/bin/liquidprompt"
    [[ -f "$liquid" ]] && source "$liquid"

    [ -x "$(command -v starship)" ] && eval "$(starship init zsh)"
}

e_nix()
{
    if [ -e /home/easbarbosa/.nix-profile/etc/profile.d/nix.sh ]; then
	. /home/easbarbosa/.nix-profile/etc/profile.d/nix.sh;
    fi
}

e_direnv()
{
    direnv hook fish | source
}

e_prompt
e_nix
e_direnv
