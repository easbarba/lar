#!/usr/bin/env zsh


# * Zsh
# Descricao: Zsh - Configuration

# * Instant Prompt


# * ALIASES

# * DEFAULT

e_settings() {
    # Command completion
    autoload -Uz compinit promptinit
    autoload -U colors && colors    # Load Colors.
    setopt globdots                 # Glob Dotfiles As Well.
    setopt extendedglob             # Use Extended Globbing.
    setopt autocd                   # Automatically Change Directory If A Directory Is Entered.

    # Smart URLs.
    autoload -Uz url-quote-magic
    zle -N self-insert url-quote-magic

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
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/zdharma-continuum/zinit/HEAD/scripts/install.sh)"
}

e_zinit_prepare()
{
    if [[ ! -f $HOME/.zinit/bin/zinit.zsh ]]; then
    print -P "%F{33}▓▒░ %F{220}Installing %F{33}DHARMA%F{220} Initiative Plugin Manager (%F{33}zdharma/zinit%F{220})…%f"
    command mkdir -p "$HOME/.zinit" && command chmod g-rwX "$HOME/.zinit"
    command git clone https://github.com/zdharma-continuum/zinit "$HOME/.zinit/bin" && \
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

e_zinit_plugins()
{
    # ESSENTIAL
    zinit light zsh-users/zsh-autosuggestions
    zinit light hlissner/zsh-autopair
    zinit light zsh-users/zsh-completions
    zinit light zdharma-continuum/fast-syntax-highlighting

    # Plugin history-search-multi-word loaded with investigating.
    zinit light zdharma-continuum/history-search-multi-word
}

e_zinit()
{
    e_zinit_prepare
    e_zinit_source
    e_zinit_plugins
}

e_prompt() {
    if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
        source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
    fi

    eval "$(starship init zsh)"
}
e_direnv() { eval "$(direnv hook zsh)"; }

# * SOURCING

# * RUN

e_settings
e_zinit
e_prompt
e_direnv
