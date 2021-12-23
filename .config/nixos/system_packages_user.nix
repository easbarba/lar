{ config, lib, pkgs, ... }:

{
  users.users.easbarbosa.packages = with pkgs;
    [
      (st.overrideAttrs (oldAttrs: rec {
        patches = [
          (fetchpatch {
            url =
              "https://st.suckless.org/patches/dracula/st-dracula-0.8.2.diff";
            sha256 = "0zpvhjg8bzagwn19ggcdwirhwc17j23y5avcn71p74ysbwvy1f2y";
          })
          # Or from any other source
          (fetchpatch {
            url =
              "https://git.sr.ht/~easbarbosa/lar/blob/main/.config/cejo/patches/st/st-bigger-fonts.patch";
            sha256 = "17dgdiwbiwipcg00xyz3yfnazfxwd6vmjphdhbw0jrnkykbhdrrm";
          })
        ];
      }))
    ];
}
