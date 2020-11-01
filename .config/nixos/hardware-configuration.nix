{ config, lib, pkgs, ... }:

{
  imports =
    [
      <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" "sr_mod" "sdhci_pci" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/39f52450-03e5-4c0a-b600-5fae023ea96e";
      fsType = "ext4";
    };

  fileSystems."/home" =
    { device = "/dev/disk/by-uuid/1070764b-e608-41ef-b90c-1d8b6f2296c4";
      fsType = "ext4";
    };

  fileSystems."/dados" =
    { device = "/dev/disk/by-uuid/08ce9a85-67f2-4efe-810b-eb0238f77f2a";
      fsType = "ext4";
    };

  fileSystems."/varios" =
    { device = "/dev/disk/by-uuid/e680f38b-c1c0-45d9-bf79-681c826b29ba";
      fsType = "ext4";
    };

  fileSystems."/boot/efi" =
    { device = "/dev/disk/by-uuid/6ACD-5890";
      fsType = "vfat";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/c7860c5e-1181-4ea5-8e8e-8e05fc3220a9";
      fsType = "ext2";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/4c0c912e-847f-47ab-a0ed-491e0176a159"; }
    ];

  nix.maxJobs = lib.mkDefault 4;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
