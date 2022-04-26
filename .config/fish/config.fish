#!/usr/bin/env fish

# * Fish Shell
# Description: Fish Shell - Configuration

# * SETTINGS

set -U fish_greeting # disable greeting

# * GLOBAL ENV VARIABLES

set -Ux PAGER less
set -Ux EDITOR micro
set -Ux VISUAL emacs
set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config

# * ALIAS
# * FUNCTIONS

function e-pip
    # if type -q python
    # PIP COMPLETION
    function __fish_complete_pip
        set -lx COMP_WORDS (commandline -o) ""
        set -lx COMP_CWORD ( \
	        math (contains -i -- (commandline -t) $COMP_WORDS)-1 \
	        )
        set -lx PIP_AUTO_COMPLETE 1
        string split \  -- (eval $COMP_WORDS[1])
    end
    complete -fa "(__fish_complete_pip)" -c pip
end

function e-completions
    if which kubectl
        kubectl completion fish | source
    end
end

function e-fisher
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish

    fisher update
end

function e-packers
    source ~/.asdf/asdf.fish
    # mkdir -p ~/.config/fish/completions; and ln -s ~/.asdf/completions/asdf.fish ~/.config/fish/completions
end

function e-direnv
    eval (direnv hook fish)
end

function e-prompt
    starship init fish | source
end

# * SOURCING

# ** RUN
# e-pip
e-direnv
e-completions
e-prompt
