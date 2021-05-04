e_direnv()
{
    eval "$(direnv hook zsh)"
}

e_nix()
{
    [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ] && . $HOME/.nix-profile/etc/profile.d/nix.sh
}

e_direnv
e_nix
