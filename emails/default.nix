{ emails ? { outPath = ./.; name = "emails"; }
, pkgs ? import <nixpkgs> {}
}:
with pkgs.lib;
let
  nodePackages = import "${pkgs.path}/pkgs/top-level/node-packages.nix" {
    inherit pkgs;
    inherit (pkgs) stdenv nodejs fetchurl fetchgit;
    neededNatives = [ pkgs.python ] ++ pkgs.lib.optional pkgs.stdenv.isLinux pkgs.utillinux;
    self = nodePackages;
    generated = ./packages.generated.nix;
  };
in rec {
  tarball = pkgs.runCommand "emails-0.0.1.tgz" { buildInputs = [ pkgs.nodejs ]; } ''
    mv `HOME=$PWD npm pack ${emails}` $out
  '';
  build = nodePackages.buildNodePackage {
    name = "emails-0.0.1";
    src = [ tarball ];
    emailSrc = ../../emails/src;
    preShellHook = ''
    '';
    buildPhase = ''
      # set -x
      ln -sfT "$emailSrc" src
      eval "$shellHook"
      npm run build
    '';
    installPhase = ''
      mkdir "$out"
      cp -r dist "$out"
      cp -r prebuild "$out"
    '';
    buildInputs = nodePackages.nativeDeps."emails" or [];
    deps = [ nodePackages.by-spec."foundation-emails"."~2.2.1" nodePackages.by-spec."babel-core"."~6.3.26" nodePackages.by-spec."babel-preset-es2015"."~6.3.13" nodePackages.by-spec."babel-register"."~6.7.2" nodePackages.by-spec."beepbeep"."~1.2.0" nodePackages.by-spec."browser-sync"."~2.11.0" nodePackages.by-spec."colors"."~1.1.2" nodePackages.by-spec."gulp"."~3.9.1" nodePackages.by-spec."gulp-awspublish"."~3.0.1" nodePackages.by-spec."gulp-cli"."~1.1.0" nodePackages.by-spec."gulp-html-src"."~1.0.0" nodePackages.by-spec."gulp-htmlmin"."~1.1.1" nodePackages.by-spec."gulp-if"."~2.0.0" nodePackages.by-spec."gulp-imagemin"."~2.4.0" nodePackages.by-spec."gulp-inline-css"."~3.0.0" nodePackages.by-spec."gulp-litmus"."0.0.7" nodePackages.by-spec."gulp-load-plugins"."~1.1.0" nodePackages.by-spec."gulp-rename"."~1.2.2" nodePackages.by-spec."gulp-replace"."~0.5.4" nodePackages.by-spec."gulp-sass"."~2.1.0" nodePackages.by-spec."gulp-sourcemaps"."~1.6.0" nodePackages.by-spec."gulp-zip"."~3.2.0" nodePackages.by-spec."inky"."~1.3.6" nodePackages.by-spec."lazypipe"."~1.0.1" nodePackages.by-spec."merge-stream"."~1.0.0" nodePackages.by-spec."panini"."~1.3.0" nodePackages.by-spec."rimraf"."~2.3.3" nodePackages.by-spec."run-sequence"."~1.2.2" nodePackages.by-spec."siphon-media-query"."~1.0.0" nodePackages.by-spec."yargs"."~4.1.0" ];
    peerDependencies = [];
  };
}
