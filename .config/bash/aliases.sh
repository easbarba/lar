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

# GNU
alias nano="nano -xcSxmkcig_ --speller='aspell'"

# Container
# docker stop `docker ps -qa`

# # Remove all containers
# docker rm `docker ps -qa`

# # Remove all images
# docker rmi -f $(docker images -qa)
# podman rmi -f $(podman images -qa)

# # Remove all volumes
# docker volume rm $(docker volume ls -qf)
# podman volume rm -f $(podman volume ls -q)

# # Remove all networks
# docker network rm $(docker network ls -q)
# podman network rm $(podman network ls -q)
