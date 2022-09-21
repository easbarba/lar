#!/usr/bin/env bash

# * ALIASES

# some more ls aliases
alias ll='ls -l'
alias la='ls -A'
alias l='ls -CF'

# ** CDing
alias ..='cd ..'
alias ...='cd ../../../'
alias mkdir='mkdir -pv'

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# GIT
alias gips='git push'
alias gipl='git pull'
alias gist='git status'
alias gilo='git log --oneline --decorate --graph'


# ** JAVA
alias j!=jbang
