{ callPackage, callCabal2nix, jdk }:
let
  pkgs = callPackage ({pkgs}: pkgs) {};
  src = builtins.filterSource
    (path: type:
      let baseName = baseNameOf path; in baseName != ".git"
    )
    ./.;
  pkg = callCabal2nix "android-activity" src {};
in
  pkgs.haskell.lib.overrideCabal pkg (drv: {
    librarySystemDepends = (drv.librarySystemDepends or []) ++ [jdk];
  })
