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

HISTSIZE=
HISTFILESIZE=
HISTFILE=~/.history
HISTTIMEFORMAT="[%F %T] "
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"
HISTCONTROL="erasedups:ignoreboth"              # Avoid duplicate entries
PROMPT_DIRTRIM=2                                # Automatically trim long paths in the prompt (requires Bash 4.x)
HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear" # Don't record some commands
HISTTIMEFORMAT='%F %T '                         # Use standard ISO 8601 timestamp
CDPATH="."                                      # This defines where cd looks for targets # Add the directories you want to have fast access to, separated by colon
HISTCONTROL=ignoredups                          #SHORTER HISTORY

# * ALIASES

# ** CDing
alias ..='cd ..'
alias ...='cd ../../../'
# alias mkdir='mkdir -pv'

# color me amazed!
for app in ls dir vdir grep fgrep egrep; do
    alias "${app}"="'${app}'  --color=auto"
done

alias s-path='echo -e ${PATH//:/\\n}' # prettier PATH entries

# * APPS
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)" # make less more friendly for non-text input files, see lesspipe(1)
[[ -x $(command -v kubectl) ]] && . <(kubectl completion bash)
[[ -x $(command -v starship) ]] && eval "$(starship init bash)"

# * GNU SCREEN
if [ -x "$(command -v tmux)" ] && [ -n "${DISPLAY}" ] && [ -z "${TMUX}" ]; then
    tmux attach || tmux >/dev/null 2>&1
fi
#[[ -x "$(command -v screen)" && -z "$STY" ]] && screen -S GNU

export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
