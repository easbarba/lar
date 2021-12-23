{ config, lib, pkgs, ... }:

{
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # base
    unzip
    curl
    lynx

    # system
    p7zip
    zip
    subversion
    jq
    lshw
    at
    usbutils
    lsof
    time
    entr
    tree
    xsel
    xclip
    fontconfig
    bc
    htop
    strace
    ltrace
    cpio
    lhasa
    lzop
    atool
    perf-tools
    sysstat
    ncdu
    rsync
    rclone
    util-linux
    unrar
    killall
    tcpdump

    # gnu
    gcc
    gnumake
    wget
    parted
    texinfo
    parallel
    automake
    autoconf

    # dev
    git
    direnv
    sqlite
    jq

    # langs
    go
    python3
    ruby
    lua
    luarocks

    # shell
    bash
    bash-completion
    shellcheck
    alacritty
    tmux
    fish
    starship
    ripgrep
    fd
    graphviz

    # infra
    podman
    buildah
    docker
    ansible
    qemu
    virt-manager
    vagrant
    virtualbox
    # ovmf bridge-utils

    # network
    iproute2
    socat
    iw
    wirelesstools
    nettools
    dig

    # editors
    emacs
    libgccjit
    zile
    neovim
    editorconfig-core-c

    # sound
    mpd
    mpc_cli
    ncmpcpp

    # system
    openssl

    # xorg
    feh
    scrot
    zathura
    slock
    gnome.cheese
    evince
    dmenu

    # wm apps
    youtube-dl
    ffmpeg
    lm_sensors
    mpv
    libnotify
    imagemagick
    pandoc
    aspell
    aria2
    udiskie
    gnome.gnome-keyring
    xdg-utils
    playerctl
    bluez
    bluez-tools
    w3m
    elinks
    dunst
    brightnessctl
    neofetch
    transmission-gtk

    # user
    firefox
    libreoffice
    gimp
    obs-studio
    amfora # netsurf kdeconnect falkon
    tdesktop
    steam
    falkon

    # nix
    nixfmt

    # codec
    vorbis-tools
    gst_all_1.gst-libav
    gst_all_1.gst-plugins-bad
    gst_all_1.gst-plugins-base
    gst_all_1.gst-plugins-good
    gst_all_1.gst-plugins-ugly

    # laptop
    acpi
    tlp
    pciutils
  ];
}
