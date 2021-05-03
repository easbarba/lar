{
  # * nix-env
  allowUnfree = true;

  # * Browsers
  #chromium = {
   # enablePepperFlash = true;
  #};

  firefox = {
    enableGoogleTalkPlugin = true;
    enableAdobeFlash = true;
  };
}
