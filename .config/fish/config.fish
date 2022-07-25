#!/usr/bin/env fish

# * SETTINGS

set -U fish_greeting # disable greeting

# * GLOBAL ENV-VARS

set -Ux PAGER less
set -Ux EDITOR micro
set -Ux VISUAL emacs
set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config

# * ALIAS

# * APPS
function s-fisher
    curl -sL https://git.io/fisher | source
    fisher update
end

if type -q python
    function __fish_complete_pip
        set -lx COMP_WORDS (commandline -o) ""
        set -lx COMP_CWORD (math (contains -i -- (commandline -t) $COMP_WORDS) -1)
        set -lx PIP_AUTO_COMPLETE 1

        string split \  -- (eval $COMP_WORDS[1])
    end

    complete -fa "(__fish_complete_pip)" -c pip
end

if command -qv kubectl
    kubectl completion fish | source
end

if command -qv starship
    starship init fish | source
end

if command -qv direnv
    direnv hook fish | source
end

# * SOURCING
