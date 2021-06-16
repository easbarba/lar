#!/usr/bin/env zsh

# * Zsh
# Descricao: Zsh - Configuration

# * ALIASES

# * DEFAULT

e_default() {
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
    [[ -n "${key[Up]}" ]] && bindkey -- "${key[Up]}"   up-line-or-beginning-search
    [[ -n "${key[Down]}" ]] && bindkey -- "${key[Down]}" down-line-or-beginning-search
}

# * ZINIT - https://github.com/zdharma/zinit
#
e_zinit_install()
{
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/zdharma/zinit/master/doc/install.sh)"
}

e_zinit_prepare()
{
    if [[ ! -f $HOME/.zinit/bin/zinit.zsh ]]; then
    print -P "%F{33}▓▒░ %F{220}Installing %F{33}DHARMA%F{220} Initiative Plugin Manager (%F{33}zdharma/zinit%F{220})…%f"
    command mkdir -p "$HOME/.zinit" && command chmod g-rwX "$HOME/.zinit"
    command git clone https://github.com/zdharma/zinit "$HOME/.zinit/bin" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
        print -P "%F{160}▓▒░ The clone has failed.%f%b"
    fi
}

e_zinit_source()
{
    source "$HOME/.zinit/bin/zinit.zsh"
    autoload -Uz _zinit
    (( ${+_comps} )) && _comps[zinit]=_zinit
}

e_zinit_begin()
{
    zinit light-mode for \
        zinit-zsh/z-a-rust \
        zinit-zsh/z-a-as-monitor \
        zinit-zsh/z-a-patch-dl \
        zinit-zsh/z-a-bin-gem-node
}

e_zinit_plugins()
{
    # ESSENTIAL 
    zinit light zsh-users/zsh-autosuggestions
    zinit light zdharma/fast-syntax-highlighting
    zinit light zsh-users/zsh-completions
 
    # Plugin history-search-multi-word loaded with investigating.
    zinit load zdharma/history-search-multi-word

    # Load the pure theme, with zsh-async library that's bundled with it.
    zinit ice pick"async.zsh" src"pure.zsh"
    zinit light sindresorhus/pure

    # LOADING
    zinit for \
        light-mode  zsh-users/zsh-completions \
        light-mode  zsh-users/zsh-autosuggestions \
        light-mode  zdharma/fast-syntax-highlighting \
        zdharma/history-search-multi-word \
        light-mode pick"async.zsh" src"pure.zsh" \
        sindresorhus/pure
}

e_zinit()
{
    e_zinit_prepare
    e_zinit_source
    e_zinit_begin
    e_zinit_plugins
}

e_prompt()
{
    [ -x $(command -v starship) ] && eval "$(starship init zsh)"
}

# * SOURCING

# * RUN

e_default | e_zinit
e_prompt
