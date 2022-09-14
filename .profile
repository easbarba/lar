# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ]; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ]; then
    PATH="$HOME/.local/bin:$PATH"
fi

# ** DOOM EMACS
export DOOM_DIR="$HOME/.config/emacs"
export PATH="$DOOM_DIR/bin:$PATH"

# ** JAVA
export PATH="$HOME/.jbang/bin:$PATH"
export SDKMAN_DIR="$HOME/.sdkman"

# ** RUBY
export GEM_HOME="$HOME/.local/gem"
export PATH="$GEM_HOME/bin:$PATH"

# ** GUIX
export GUIX_PROFILE="$HOME/.guix-profile"
export GUIX_LOCPATH="$GUIX_PROFILE/lib/locale"
export PATH="$GUIX_PROFILE/bin:$PATH"
