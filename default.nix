let
  enableProfiling = false;
  pkgs = import ./nixpkgs { config.allowUnfree = true; };
  overrideCabal = drv: f: (drv.override (args: args // {
    mkDerivation = drv: args.mkDerivation (drv // f drv);
  })) // {
    overrideScope = scope: overrideCabal (drv.overrideScope scope) f;
  };
  sharedOverrides = self: super: {
    reflex = self.callPackage ./reflex {};
    reflex-dom = self.callPackage ./reflex-dom {};
    heist = overrideCabal super.heist (drv: {
      revision = "2"; # To allow filePath >= 1.4
      editedCabalFile = "0d4x6vsp4mrkbljavcgfvplc4xsmfr6qn4b889j77achj3z4rkkk";
    });
    timezone-series = overrideCabal super.timezone-series (drv: {
      jailbreak = true; # To allow time >= 1.5
    });
    timezone-olson = overrideCabal super.timezone-olson (drv: {
      jailbreak = true; # To allow time >= 1.5
    });
    HList = overrideCabal super.HList (drv: {
      version = "0.4.0.0";
      sha256 = "0f6d97vfxlml4dp6zfk95kk4la8xr5m91hiw4zj98kvwvvhb99mz";
      buildDepends = drv.buildDepends ++ [self.profunctors];
    });
    focus-core = self.mkDerivation ({
      pname = "focus-core";
      license = null;
      version = "0.1";
      src = ./core;
      buildDepends = with self; [
        aeson
        attoparsec
        base64-bytestring
        stripe
        text
        time
        vector
        network-uri
        timezone-series
        constraints
        dependent-map
        reflex
        HList
      ];
    });
    dependent-sum-template = overrideCabal super.dependent-sum-template (drv: {
      version = "0.0.0.3";
      sha256 = "0if3mr0cmaz3yc0hbn0fpx14kwnjsaj3hd8mw9z4va4qp85wya69";
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
  };

  backendHaskellPackagesBase = if enableProfiling then pkgs.haskell-ng.packages.ghc7101 else pkgs.haskell-ng.packages.ghc7101; /* TODO re-add profiling */
  frontendHaskellPackagesBase = pkgs.haskell-ng.packages.ghcjs;
  extendFrontendHaskellPackages = haskellPackages: haskellPackages.override {
    overrides = self: super: sharedOverrides self super // {
      ghcjs-canvas = self.mkDerivation ({
        pname = "ghcjs-canvas";
        license = null;
        src = ./ghcjs-canvas;
        buildDepends = with self; [ text ghcjs-base base ];
        version = "0.1.0.0";
      });
      stripe = self.mkDerivation ({
        pname = "stripe";
        license = null;
        src = ./hs-stripe;
        buildDepends = with self; [ aeson http-conduit http-types mtl text unordered-containers utf8-string ghcjs-base ghcjs-dom reflex-dom ];
        version = "0.8.3";
      });
      focus-js = self.mkDerivation ({
        license = null;
        pname = "focus-js";
        version = "0.1";
        src = ./js;
        buildDepends = with self; [ focus-core reflex reflex-dom aeson attoparsec text time vector ghcjs-base ghcjs-dom constraints ghcjs-canvas ];
      });
      crypto-numbers = self.mkDerivation ({
        pname = "crypto-numbers";
        version = "0.2.2";
        sha256 = "1ia39al01hb65h23ql0mr5vwzj8slv98i7a22cix8p0b6an1w3vv";
        buildDepends = with self; [ crypto-random vector ];
        testDepends = with self; [
          byteable crypto-random HUnit QuickCheck test-framework
          test-framework-hunit test-framework-quickcheck2 vector
        ];
        homepage = "http://github.com/vincenthz/hs-crypto-numbers";
        description = "Cryptographic numbers: functions and algorithms";
        license = self.stdenv.lib.licenses.bsd3;
        platforms = self.ghc.meta.platforms;
      });
    };
  };
  frontendHaskellPackages = extendFrontendHaskellPackages frontendHaskellPackagesBase;

  extendBackendHaskellPackages = haskellPackages: haskellPackages.override {
    overrides = self: super: sharedOverrides self super // {
      groundhog = self.mkDerivation ({
        pname = "groundhog";
        version = "0.7.0.3";
        sha256 = "0n5c501wfyqcl1iy4017yyxp95kz7mb4lgc0mjjk9si36ixkww9r";
        buildDepends = with self; [
          aeson attoparsec base64-bytestring blaze-builder monad-control
          monad-logger mtl scientific text time transformers transformers-base
        ];
        homepage = "http://github.com/lykahb/groundhog";
        description = "Type-safe datatype-database mapping library";
        license = self.stdenv.lib.licenses.bsd3;
        platforms = self.ghc.meta.platforms;
      });
      groundhog-th = overrideCabal super.groundhog-th (drv: {
        src = ./groundhog/groundhog-th;
      });
      snap-loader-static = overrideCabal super.snap-loader-static (drv: {
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
        license = self.stdenv.lib.licenses.bsd3;
        platforms = self.ghc.meta.platforms;
      });
      snap = self.mkDerivation ({
        pname = "snap";
        version = "0.14.0.2";
        revision = "1";
        sha256 = "1yv1snkibsqd7cdxyqi7c8gvnv1hzzhw5jlk19kps526n5xvay7r";
        editedCabalFile = "1640756ec7bfd3130869dce451904d6cc762ab6c8b8128982933fba80f325c92";
        isLibrary = true;
        isExecutable = true;
        buildDepends = with self; [
          aeson attoparsec base bytestring cereal clientsession comonad
          configurator containers directory directory-tree dlist errors
          filepath hashable heist lens logict MonadCatchIO-transformers mtl
          mwc-random old-time pwstore-fast regex-posix snap-core snap-server
          stm syb template-haskell text time transformers
          unordered-containers vector vector-algorithms xmlhtml
        ];
        homepage = "http://snapframework.com/";
        description = "Top-level package for the Snap Web Framework";
        license = pkgs.stdenv.lib.licenses.bsd3;
      });
      stripe = self.mkDerivation ({
        pname = "stripe";
        license = null;
        src = ./hs-stripe;
        buildDepends = with self; [ aeson http-conduit http-types mtl text unordered-containers utf8-string ];
        version = "0.8.3";
      });
      focus-backend = backendHaskellPackages.mkDerivation ({
        pname = "focus-backend";
        license = null;
        version = "0.1";
        src = ./backend;
        buildDepends = with self; [
          groundhog
          groundhog-th
          mtl
          focus-core
          lens
          aeson
          snap
          resource-pool
          text
          network
          stm
          postgresql-simple
          groundhog-postgresql
          websockets-snap
          websockets
          stripe
          smtp-mail
          temporary
          stringsearch
          shelly
        ];
        pkgconfigDepends = [
          pkgs.postgresql94
        ];
      });
      singletons = self.mkDerivation ({
        pname = "singletons";
        version = "1.1.1";
        sha256 = "1pbz42i2vxmw3sf3f4sqvgyp9a1b1q5my7xq64h37a9g6jd2246a";
        buildDepends = with self; [ mtl th-desugar ];
        homepage = "http://www.cis.upenn.edu/~eir/packages/singletons";
        description = "A framework for generating singleton types";
        license = self.stdenv.lib.licenses.bsd3;
        platforms = self.ghc.meta.platforms;
        jailbreak = true;
        doCheck = false;
        doHaddock = false;
      });
    };
  };
  backendHaskellPackages = extendBackendHaskellPackages backendHaskellPackagesBase;
in
{ name
, version
, backendDepends ? (p: [])
, backendTools ? (p: [])
, frontendDepends ? (p: [])
, frontendTools ? (p: [])
, commonDepends ? (p: [])
, commonTools ? (p: [])
}:
let
  # Break recursion
  appName = name;
  appVersion = version;

  libraryHeader = ''
    library
      exposed-modules: $(cd src ; find * -iname '[A-Z]*.hs' | sed 's/\.hs$//' | tr / . | tr "\n" , | sed 's/,$//')
  '';
  executableHeader = executableName: ''
    executable ${executableName}
      main-is: $(cd src; ls | grep -i '^\(${executableName}\|main\)\.\(l\|\)hs'$)
  '';
  #TODO: The list of builtin packages should be in nixpkgs, associated with the compiler
  mkPreConfigure = pname: executableName: depends: ''
    if ! ls | grep ".*\\.cabal$" ; then
      cat >"${pname}.cabal" <<EOF
    name: ${pname}
    version: ${appVersion}
    cabal-version: >= 1.2
    ${if executableName != null then executableHeader executableName else libraryHeader}
      hs-source-dirs: src
      build-depends: ${pkgs.lib.concatStringsSep "," ([ "base" "bytestring" "containers" "time" "transformers" "text" "lens" "aeson" "mtl" "directory" "deepseq" ] ++ builtins.filter (x: x != null) (builtins.map (x: x.pname or null) depends))}
      other-extensions: TemplateHaskell
      ghc-options: -threaded -Wall -fwarn-tabs -funbox-strict-fields -O2 -fprof-auto-calls -rtsopts
    EOF
    fi
  '';

  common = haskellPackages: haskellPackages.mkDerivation (rec {
    pname = "${appName}-common";
    version = appVersion;
    src = ../common;
    license = null;
    preConfigure = mkPreConfigure pname null buildDepends;
    buildDepends = with haskellPackages; [
      focus-core
    ] ++ commonDepends haskellPackages;
    buildTools = [] ++ commonTools pkgs;
  });
  mkFrontend = haskellPackages:
    with haskellPackages;
    let
      frontendCommon = common haskellPackages;
      in haskellPackages.mkDerivation (rec {
        pname = "${appName}-frontend";
        version = appVersion;
        license = null;
        src = ../frontend;
        preConfigure = mkPreConfigure pname "frontend" buildDepends;
        buildDepends = [
          frontendCommon
          focus-core
          focus-js
        ] ++ frontendDepends haskellPackages;
        buildTools = [] ++ frontendTools pkgs;
        isExecutable = true;
        passthru = {
          common = frontendCommon;
          inherit haskellPackages;
        };
    });
in pkgs.stdenv.mkDerivation (rec {
  name = "${appName}-${appVersion}";
  static = ../static;
  builder = builtins.toFile "builder.sh" ''
    source $stdenv/setup

    mkdir -p $out
    cp -r $static $out/static
    ln -s $backend/bin/backend $out
    ln -st $out $frontend/bin/*
  '';
  backend =
    with backendHaskellPackages;
    let
      backendCommon = common backendHaskellPackages;
    in backendHaskellPackages.mkDerivation (rec {
      pname = "${appName}-backend";
      license = null;
      version = appVersion;
      src = ../backend;
      preConfigure = mkPreConfigure pname "backend" buildDepends;
      preBuild = ''
        ln -sf ${pkgs.tzdata}/share/zoneinfo .
      '';
      buildDepends = [
        backendCommon
        focus-core focus-backend
      ] ++ backendDepends backendHaskellPackages;
      buildTools = [] ++ backendTools pkgs;
      isExecutable = true;
      configureFlags = [ "--ghc-option=-lgcc_s" ] ++ (if enableProfiling then [ "--enable-executable-profiling" ] else [ ]);
      passthru = {
        common = backendCommon;
        haskellPackages = backendHaskellPackages;
      };
    });
  frontend = mkFrontend frontendHaskellPackages;
  passthru = {
    frontendGhc = mkFrontend backendHaskellPackages;
  };
})
