#!/usr/bin/env zsh

# * ALIASES

# * DEFAULT CONFIG

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


# * ZINIT - https://github.com/zdharma/zinit
# ZINIT | INSTALL
if [ -f "$HOME/.zinit/bin/zinit.zsh" ]; then
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/zdharma-continuum/zinit/HEAD/scripts/install.sh)"
fi

# ** ZINIT | PREPARING
if [ ! -f "$HOME/.zinit/bin/zinit.zsh" ]; then
    print -P "%F{33}▓▒░ %F{220}Installing %F{33}DHARMA%F{220} Initiative Plugin Manager (%F{33}zdharma/zinit%F{220})…%f"
    command mkdir -p "$HOME/.zinit" && command chmod g-rwX "$HOME/.zinit"
    command git clone https://github.com/zdharma-continuum/zinit "$HOME/.zinit/bin" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
        print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi


# ** ZINIT | SOURCING
if [ ! -f "$HOME/.zinit/bin/zinit.zsh" ]; then
    source "$HOME/.zinit/bin/zinit.zsh"
    autoload -Uz _zinit
    (( ${+_comps} )) && _comps[zinit]=_zinit
fi

# ** ZINIT | PLUGINS
if [ ! -f "$HOME/.zinit/bin/zinit.zsh" ]; then
    # ESSENTIAL
    zinit light zsh-users/zsh-autosuggestions
    zinit light hlissner/zsh-autopair
    zinit light zsh-users/zsh-completions
    zinit light zdharma-continuum/fast-syntax-highlighting

    # Plugin history-search-multi-word loaded with investigating.
    zinit light zdharma-continuum/history-search-multi-word
fi

# * PROMPTS
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
    source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi
[ -x $(command -v starship) ] && eval "$(starship init zsh)"


# * APPS
# [ -x $(command -v direnv) ] && eval "$(direnv hook zsh)"
# [ -x $(command -v asdf) ] && . $HOME/.asdf/asdf.sh
# [ -x $(command -v kubectl) ] && source <(kubectl completion zsh)
# if [ -x $(command -v eksctl) ]; then
#     mkdir -p ~/.zsh/completion/
#     eksctl completion zsh > ~/.zsh/completion/_eksctl
# fi


# * SOURCING
### End of Zinit's installer chunk
