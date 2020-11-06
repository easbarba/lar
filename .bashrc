#!/usr/bin/env bash

# * Description: GNU Bash - CONFIGURATIONS, ALIASES...

# * CONFIGURATION

set -o noclobber # Prevent file overwrite on stdout redirection Use `>|` to force redirection to an existing file
bind Space:magic-space # Enable history expansion with space # E.g. typing !!<space> will replace the !! with your last command
bind "set completion-ignore-case on" # SMARTER TAB-COMPLETION (Readline bindings) Perform file completion in a case insensitive fashion
bind "set completion-map-case on" # Treat hyphens and underscores as equivalent
bind "set show-all-if-ambiguous on" # Display matches for ambiguous patterns at first tab press
bind "set mark-symlinked-directories on" # Immediately add a trailing slash when autocompleting symlinks to directories
bind '"\eh": "\C-a\eb\ed\C-y\e#man \C-y\C-m\C-p\C-p\C-a\C-d\C-e"' # MIMIC ZSH RUN-HELP ABILITY
shopt -s histappend # Append to the history file, don't overwrite it
shopt -s checkwinsize # Update window size after every command
shopt -s cmdhist # Save multi-line commands as one command
shopt -s autocd 2> /dev/null # Prepend cd to directory names automatically
shopt -s dirspell 2> /dev/null # Correct spelling errors during tab-completion
shopt -s cdspell 2> /dev/null # Correct spelling errors in arguments supplied to cd
shopt -s globstar 2> /dev/null # Turn on recursive globbing (enables ** to recurse all directories)
shopt -s nocaseglob; # Case-insensitive globbing (used in pathname expansion)
shopt -s nocasematch # Match disable case sensitivity
shopt -s cdable_vars # Bookmark your favorite places across the file system
PROMPT_COMMAND='history -a' # Record each line as it gets issued
bind '"\e[A": history-search-backward' # history completion
bind '"\e[B": history-search-forward'
bind '"\e[C": forward-char'
bind '"\e[D": backward-char'

HISTSIZE=
HISTFILESIZE=
HISTFILE=~/.history
HISTTIMEFORMAT="[%F %T] "
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"
HISTCONTROL="erasedups:ignoreboth" # Avoid duplicate entries
PROMPT_DIRTRIM=2 # Automatically trim long paths in the prompt (requires Bash 4.x)
HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear" # Don't record some commands
HISTTIMEFORMAT='%F %T ' # Use standard ISO 8601 timestamp
CDPATH="." # This defines where cd looks for targets # Add the directories you want to have fast access to, separated by colon
HISTCONTROL=ignoredups #SHORTER HISTORY

# * ALIASES

# directories
alias ..='cd ..'
alias ...='cd ../../../'
alias mkdir='mkdir -pv'

# grep
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

# system
alias _=sudo

# colorful
alias ls='ls --color=auto'
alias dir='dir --color=auto'
alias vdir='vdir --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# misc
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias j="jump"
alias c="clear"
alias lx-reload='source $HOME/.bashrc; echo "Shell Reloaded"'

# * SYSTEM SCRIPTS

[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)" # make less more friendly for non-text input files, see lesspipe(1)

if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then # set variable identifying the chroot you work in
    debian_chroot=$(cat /etc/debian_chroot)
fi

if ! shopt -oq posix; then # BASH COMPLETION
    if [ -f /usr/share/bash-completion/bash_completion ]; then
	. /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
	. /etc/bash_completion
    fi
fi

# * CLI SOFTWARE

e-inhouse-scripts()
{
    local shell_files="$HOME/.config/bash";
    [[ ! -d $shell_files ]] && return

    source "$shell_files/shell-paths"
    source "$shell_files/shell-utils"
    source "$shell_files/shell-funcs"
    source "$shell_files/shell-packages"
    source "$shell_files/shell-distro"
}

e-prompt()
{
    [[ $(command -v starship) ]] && eval "$(starship init bash)"

    local liquid="$HOME/bin/liquidprompt"
    [[ ! $(command -v starship) ]] && [[ -f $liquid ]] && source "$liquid"
}

e-cli-tools()
{
    local enhancd_dir="$HOME/Projects/Bash/enhancd/"
    [[ -d $enhancd_dir ]] && cd $enhancd_dir && source ./init.sh
}

e-multiplexers()
{
    if [[ "$DISPLAY" ]]; then
	if [[ -x "$(command -v tmux)" ]] && test -z "$TMUX"; then
	    tmux attach || tmux new-session
	fi
    else
	[[ -x "$(command -v screen)" ]] && screen -RR Session
    fi
}


# * RUN

e-inhouse-scripts

e-prompt
e-cli-tools
e-multiplexers

# e-basher()
# {
#     local basher_dir="$HOME/.basher"

#     [[ "$1" == 'upgrade' ]] && git -C "$HOME/.config/basher" pull

#     [[ ! -d "$basher_dir/.git" ]] && git clone https://github.com/basherpm/basher "$basher_dir"

#     # add Basher Bin folder to $PATH
#     export PATH="$basher_dir/bin"${PATH:+:}${PATH}

#     # load basher
#     eval "$(basher init - bash)"
# }
