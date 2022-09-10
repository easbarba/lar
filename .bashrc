#!/usr/bin/env bash

# * Description: GNU Bash - CONFIGURATIONS, ALIASES...

# * CONFIGURATION

set -o noclobber                                                  # Prevent file overwrite on stdout redirection Use `>|` to force redirection to an existing file
bind Space:magic-space                                            # Enable history expansion with space # E.g. typing !!<space> will replace the !! with your last command
bind "set completion-ignore-case on"                              # SMARTER TAB-COMPLETION (Readline bindings) Perform file completion in a case insensitive fashion
bind "set completion-map-case on"                                 # Treat hyphens and underscores as equivalent
bind "set show-all-if-ambiguous on"                               # Display matches for ambiguous patterns at first tab press
bind "set mark-symlinked-directories on"                          # Immediately add a trailing slash when autocompleting symlinks to directories
bind '"\eh": "\C-a\eb\ed\C-y\e#man \C-y\C-m\C-p\C-p\C-a\C-d\C-e"' # MIMIC ZSH RUN-HELP ABILITY
shopt -s histappend                                               # Append to the history file, don't overwrite it
shopt -s checkwinsize                                             # Update window size after every command
shopt -s cmdhist                                                  # Save multi-line commands as one command
shopt -s autocd 2>/dev/null                                       # Prepend cd to directory names automatically
shopt -s dirspell 2>/dev/null                                     # Correct spelling errors during tab-completion
shopt -s cdspell 2>/dev/null                                      # Correct spelling errors in arguments supplied to cd
shopt -s globstar 2>/dev/null                                     # Turn on recursive globbing (enables ** to recurse all directories)
shopt -s nocaseglob                                               # Case-insensitive globbing (used in pathname expansion)
shopt -s nocasematch                                              # Match disable case sensitivity
shopt -s cdable_vars                                              # Bookmark your favorite places across the file system
PROMPT_COMMAND='history -a'                                       # Record each line as it gets issued
bind '"\e[A": history-search-backward'                            # history completion
bind '"\e[B": history-search-forward'
bind '"\e[C": forward-char'
bind '"\e[D": backward-char'

HISTSIZE=1000
HISTFILESIZE=2000
HISTFILE=~/.history
HISTTIMEFORMAT="[%F %T] "
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"
HISTCONTROL="erasedups:ignoreboth"              # Avoid duplicate entries
PROMPT_DIRTRIM=2                                # Automatically trim long paths in the prompt (requires Bash 4.x)
HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear" # Don't record some commands
HISTTIMEFORMAT='%F %T '                         # Use standard ISO 8601 timestamp
CDPATH="."                                      # This defines where cd looks for targets # Add the directories you want to have fast access to, separated by colon
HISTCONTROL=ignoreboth                          # don't put duplicate lines or lines starting with space in the history.

# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return ;;
esac

# See bash(1) for more options

# append to the history file, don't overwrite it
shopt -s histappend

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color | *-256color) color_prompt=yes ;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
        # We have color support; assume it's compliant with Ecma-48
        # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
        # a case would tend to support setf rather than setaf.)
        color_prompt=yes
    else
        color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
    xterm* | rxvt*)
        PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
        ;;
    *) ;;

esac

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
fi
# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return ;;
esac

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
#[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color | *-256color) color_prompt=yes ;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
        # We have color support; assume it's compliant with Ecma-48
        # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
        # a case would tend to support setf rather than setaf.)
        color_prompt=yes
    else
        color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
    xterm* | rxvt*)
        PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
        ;;
    *) ;;

esac

[[ -f ~/.bash_aliases ]] && . ~/.bash_aliases

# color me amazed!
for app in ls dir vdir grep fgrep egrep; do
    alias "${app}"="'${app}'  --color=auto"
done

alias s-path='echo -e ${PATH//:/\\n}' # prettier PATH entries

# * APPS
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)" # make less more friendly for non-text input files, see lesspipe(1)
[[ -x $(command -v starship) ]] && eval "$(starship init bash)"
if [[ -x "$(command -v tmux)" ]] && [[ -n "${DISPLAY}" ]] && [[ -z "${TMUX}" ]]; then tmux attach || tmux >/dev/null 2>&1; fi
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
#[[ -x "$(command -v screen)" && -z "$STY" ]] && screen -S GNU
