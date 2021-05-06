{ pkgs }:
let
  inherit (pkgs) lib buildEnv;

in {
  # * nix-env
  allowUnfree = true;

  # * Browsers
  chromium = {
    enableWideVine = true; # enable the DRM to allow things like Netflix to work
  };

  firefox = {
    enableGoogleTalkPlugin = true;
    enableAdobeFlash = true;
  };

  packageOverrides = pkgs: {
    desktopDev = lib.lowPrio (buildEnv {
      name = "bagaceira";
      paths = with pkgs; [
        ruby
        solargraph
        libyaml
        elixir
        nodejs
      ];
    });
  };
}
