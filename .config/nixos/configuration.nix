{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # *  Boot Loader.
  boot.loader = {
	  efi = {
	    canTouchEfiVariables = true;
	    efiSysMountPoint = "/boot/efi"; # ← use the same mount point here.
	  };
	  grub = {
		  efiSupport = true;
		  device = "nodev";
	  };
  };

  # * Misc
  nixpkgs.config.allowUnfree = true;

  # * Networking
  networking.hostName = "nixos";
  networking.wireless.enable = true;
  networking.wireless.extraConfig = ''
    ctrl_interface=/run/wpa_supplicant
    ctrl_interface_group=wheel
    '';
  networking.useDHCP = false;
  networking.interfaces.enp0s25.useDHCP = true;
  networking.interfaces.wlp3s0.useDHCP = true;

  # * Time
  time.timeZone = "America/Sao_Paulo";

  # * Environment
  environment.variables.EDITOR = "konsole";

  # * Global Packages
  environment.systemPackages = with pkgs; [
	  wget coreutils gcc gnumake gnufdisk gnutar parted zile bash bash-completion
    p7zip zip unzip git
    wpa_supplicant
    tmux
	];

  # * Fonts
  fonts.fonts = with pkgs; [
    dejavu_fonts jetbrains-mono freefont_ttf
    source-han-sans-japanese source-han-sans-korean
  ];

	# * SSH
	services.openssh.enable = true;

	# * Sound
	sound.enable = true;
	hardware.pulseaudio.enable = true;

	# * Laptop
	services.xserver.libinput.enable = true;

  # * X11 & Window Manager
  services.xserver.enable = true;
	services.xserver.layout = "us";

	# * Window Manager
	services.xserver.autorun = true;
	services.xserver.displayManager.lightdm.enable = true;
  # security.pam.services.lightdm.enable = true;
	services.xserver.windowManager.awesome.enable = true;
  services.gnome3.gnome-keyring.enable = true;
  programs.seahorse.enable = true;

	# * User
	users.users.elxbarbosa = {
	  isNormalUser = true;
	  extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
	};
  users.extraUsers.elxbarbosa.extraGroups = [ "wheel" ];

	# This value determines the NixOS release from which the default
	# settings for stateful data, like file locations and database versions
	# on your system were taken. It‘s perfectly fine and recommended to leave
	# this value at the release version of the first install of this system.
	# Before changing this value read the documentation for this option
	# (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
	system.stateVersion = "20.03"; # Did you read the comment?
}
