{ nixpkgs }:

with nixpkgs.haskell.lib;
let inherit (nixpkgs) stdenv;
in
self: super: {
    attoparsec-enumerator = overrideCabal super.attoparsec-enumerator (drv: {
      version = "0.3.4";
      sha256 = "127mj0v6342mzxnc73qki3k197vhwsff8qkf92gm5idyxdisg5dy";
    });
    heist = overrideCabal super.heist (drv: {
      jailbreak = true;
    });
    snap = overrideCabal super.snap (drv: {
      preConfigure = ''
        sed -i 's/\(lens.*<.*4\)\.14$/\1.15/' *.cabal
      '';
      jailbreak = true;
    });
    snap-core = overrideCabal super.snap-core (drv: {
      jailbreak = true;
    });
    snap-server = overrideCabal super.snap-server (drv: {
      version = "0.9.5.1";
      sha256 = "18ryin6f315picrs2159sn2668266l3xchs7jb8isw0gp52273xg";
      revision = "1";
      editedCabalFile = "0p5apya7gd8kbkknpzamvnc902jdlp8kdmwrqzrj6gvxkr9ss2br";
      jailbreak = true;
    });
    timezone-series = overrideCabal super.timezone-series (drv: {
      jailbreak = true; # To allow time >= 1.5
    });
    timezone-olson = overrideCabal super.timezone-olson (drv: {
      jailbreak = true; # To allow time >= 1.5
    });
    groundhog = self.mkDerivation ({
      pname = "groundhog";
      version = "0.7.0.3";
      src = ./groundhog/groundhog;
      jailbreak = true;
      configureFlags = [ "--ghc-option=-XUndecidableSuperClasses" ];
      buildDepends = with self; [
        aeson attoparsec base64-bytestring blaze-builder monad-control
        monad-logger mtl scientific text time transformers transformers-base
      ];
      homepage = "http://github.com/lykahb/groundhog";
      description = "Type-safe datatype-database mapping library";
      license = nixpkgs.stdenv.lib.licenses.bsd3;
      platforms = self.ghc.meta.platforms;
    });
    groundhog-th = overrideCabal super.groundhog-th (drv: {
      src = ./groundhog/groundhog-th;
      sha256 = null;
      revision = null;
      editedCabalFile = null;
    });
    groundhog-postgresql = overrideCabal super.groundhog-postgresql (drv: {
      src = ./groundhog/groundhog-postgresql;
      sha256 = null;
      revision = null;
      editedCabalFile = null;
    });
    snap-loader-static = overrideCabal super.snap-loader-static (drv: {
      jailbreak = true;
    });
    blaze-html = overrideCabal super.blaze-html (drv: {
      jailbreak = true;
    });
    blaze-markup = overrideCabal super.blaze-markup (drv: {
      jailbreak = true;
    });
    th-desugar = self.mkDerivation ({
      pname = "th-desugar";
      version = "1.5";
      sha256 = "18ailfvwiljscyzjxci6k9h05kf9wwb6dy3ms6q928cr80qnr4d5";
      buildDepends = with self; [ mtl syb th-lift ];
      testDepends = with self; [ hspec HUnit mtl syb th-lift ];
      homepage = "http://www.cis.upenn.edu/~eir/packages/th-desugar";
      description = "Functions to desugar Template Haskell";
      license = nixpkgs.stdenv.lib.licenses.bsd3;
      platforms = self.ghc.meta.platforms;
    });
    stripe = self.mkDerivation ({
      pname = "stripe";
      license = null;
      src = ./hs-stripe;
      buildDepends = with self; [ aeson http-conduit http-types mtl text unordered-containers utf8-string ghcjs-dom reflex-dom ];
      version = "0.8.3";
    });
    singletons = self.mkDerivation ({
      pname = "singletons";
      version = "1.1.1";
      sha256 = "1pbz42i2vxmw3sf3f4sqvgyp9a1b1q5my7xq64h37a9g6jd2246a";
      buildDepends = with self; [ mtl th-desugar ];
      homepage = "http://www.cis.upenn.edu/~eir/packages/singletons";
      description = "A framework for generating singleton types";
      license = nixpkgs.stdenv.lib.licenses.bsd3;
      platforms = self.ghc.meta.platforms;
      jailbreak = true;
      doCheck = false;
      doHaddock = false;
    });
    amazonka = overrideCabal super.amazonka (drv: {
      version = "0.3.4";
      src = ./amazonka/amazonka;
    });
    amazonka-core = overrideCabal super.amazonka-core (drv: {
      version = "0.3.4";
      src = ./amazonka/core;
    });
    amazonka-ec2 = overrideCabal super.amazonka-ec2 (drv: {
      version = "0.3.4";
      src = ./amazonka/amazonka-ec2;
    });
    amazonka-s3 = overrideCabal super.amazonka-s3 (drv: {
      version = "0.3.4";
      src = ./amazonka/amazonka-s3;
    });
    amazonka-route53 = overrideCabal super.amazonka-route53 (drv: {
      version = "0.3.4";
      src = ./amazonka/amazonka-route53;
    });
    amazonka-cloudwatch = overrideCabal super.amazonka-cloudwatch (drv: {
      version = "0.3.4";
      src = ./amazonka/amazonka-cloudwatch;
    });
    amazonka-iam = overrideCabal super.amazonka-iam (drv: {
      version = "0.3.4";
      src = ./amazonka/amazonka-iam;
    });
    amazonka-sts = overrideCabal super.amazonka-sts (drv: {
      version = "0.3.4";
      src = ./amazonka/amazonka-sts;
    });
    JuicyPixels = overrideCabal super.JuicyPixels (drv: {
      jailbreak = true;
    });
    diagrams-svg = overrideCabal super.diagrams-svg (drv: {
      version = "1.3.1.10";
      src = nixpkgs.fetchgit {
        url = git://github.com/diagrams/diagrams-svg;
        rev = "0fcfe833844baccd567bc01986ffe0462f2c2d18";
        sha256 = "363e271745b9d5b4985495333866ad98c89c8d40ce972c376efeb6cc64b41140";
      };
    });
    lucid-svg = overrideCabal super.lucid-svg (drv: {
      version = "0.5.0.0";
      sha256 = "1p7ipdy0nmqfg1b038a1b5nd3xh2779d2gnw4h683mm5jcbf0mvj";
    });
    gpx-conduit = overrideCabal super.gpx-conduit (drv: {
      src = nixpkgs.fetchgit {
        url = git://github.com/obsidiansystems/gpx-conduit;
        rev = "988d97c433042e5af46ae7c38e1163eecbf928bb";
        sha256 = "9cd3d6b3c27f9a0ae5c02769001d56f773efab9657d50b948d4355397950f28c";
      };
    });
    map-syntax = doJailbreak super.map-syntax;
    websockets = doJailbreak super.websockets;
    zlib-enum = doJailbreak super.zlib-enum;
    reflex-jsx = self.callPackage ./reflex-jsx.nix {};
  }
