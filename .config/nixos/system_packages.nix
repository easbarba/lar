{ config, lib, pkgs, ... }:

{
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
     # base
     unzip curl lynx

     # system
     p7zip zip subversion jq lshw at usbutils lsof time entr tree xsel
     xclip fontconfig bc htop strace ltrace cpio lhasa lzop atool perf-tools
     sysstat ncdu rsync rclone util-linux

     # gnu
     wget parted texinfo parallel automake autoconf

     # dev
     git direnv sqlite jq

     # langs
     go ruby lua luarocks

     # shell
     bash bash-completion shellcheck alacritty tmux fish starship

     # infra
     podman docker qemu virt-manager vagrant # ovmf bridge-utils

     # network
     iproute2 socat iw wirelesstools nettools

     # editors
     emacs zile neovim

     # sound
     mpd mpc_cli ncmpcpp

     # system
     wget openssl

     # xorg
     feh scrot zathura slock gnome.cheese mate.atril dmenu

     # wm apps
     youtube-dl ffmpeg lm_sensors mpv libnotify imagemagick pandoc aspell
     aria2 udiskie gnome.gnome-keyring xdg-utils playerctl bluez bluez-tools
     w3m elinks dunst

     # user
     falkon firefox libreoffice gimp obs-studio amfora # steam netsurf
     kdeconnect

     # codec
     vorbis-tools gst_all_1.gst-libav gst_all_1.gst-plugins-bad
     gst_all_1.gst-plugins-base gst_all_1.gst-plugins-good gst_all_1.gst-plugins-ugly

     # laptop
     acpi tlp
  ];
}
