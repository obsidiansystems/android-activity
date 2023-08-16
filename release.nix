let rp = import ./reflex-platform {};
    addJdk = pkg: rp.nixpkgs.haskell.lib.overrideCabal pkg (drv: {
      librarySystemDepends = (drv.librarySystemDepends or []) ++ [rp.nixpkgs.jdk];
    });

in
  addJdk (rp.ghcAndroidAarch64.callCabal2nix "android-activity" (builtins.fetchGit ./.) {
    log = null;
  })
