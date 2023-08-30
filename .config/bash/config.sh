#!/usr/bin/bash

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

# * APPS
[[ -x $(command -v starship) ]] && eval "$(starship init bash)"
[[ -x "$(command -v tmux)" ]] && [[ -n "${DISPLAY}" ]] && [[ -z "${TMUX}" ]] && exec tmux attach || tmux >/dev/null 2>&1 # tmux new-session -A -s ${USER} >/dev/null 2>&1 #
[[ -x $(command -v kubectl) ]] && source <(kubectl completion bash)
[[ -f "$GUIX_PROFILE/etc/profile" ]] && . "$GUIX_PROFILE/etc/profile"
# [[ -x $(command -v direnv) ]] && eval "$(direnv hook bash)"

if [ -n "$GUIX_ENVIRONMENT" ]; then
    if [[ $PS1 =~ (.*)"\\$" ]]; then
        PS1="${BASH_REMATCH[1]} [env]\\\$ "
    fi
fi
