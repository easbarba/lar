#!/usr/bin/env fish

# * Fish Shell
# Description: Fish Shell - Configuration

# * SETTINGS

set -U fish_greeting

# * GLOBAL VARIABLES

set -Ux PAGER less
set -Ux EDITOR nvim
set -Ux VISUAL emacs

# * ALIAS

alias lxReload="source ~/.config/fish/config.fish"

# * FUNCTIONS

# Cero
function ceroit -d "Cero Package"
    cd /dados/Pessoal/cero/
    bundle install
    rake install
end

# * SOURCING

if type -q starship
    starship init fish | source
end

if type -q rbenv
    status --is-interactive; and rbenv init - | source
end

if test -e ~/.config/asdf/completions/asdf.fish
    cp ~/.config/asdf/completions/asdf.fish ~/.config/fish/completions
end

if type -q python
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


# * FISHER
if not functions -q fisher
    set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
end

cd "$HOME"
