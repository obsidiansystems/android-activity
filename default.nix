let
  enableProfiling = false;
  tryReflex = import ./try-reflex {
    config.packageOverrides = pkgs: rec {
      webkitgtk24x = pkgs.stdenv.lib.overrideDerivation pkgs.webkitgtk24x (oldAttrs: {
        patches = oldAttrs.patches ++ [
          ./add-webkit_dom_node_to_js.patch
        ];
      });
    };
  };
  pkgs = tryReflex.nixpkgs;
  inherit (pkgs) stdenv;
  overrideCabal = drv: f: if isNull drv then null else (drv.override (args: args // {
    mkDerivation = drv: args.mkDerivation (drv // f drv);
  })) // {
    overrideScope = scope: overrideCabal (drv.overrideScope scope) f;
  };
  sharedOverrides = self: super: {
    reflex-dom = self.callPackage ./reflex-dom {};
    attoparsec-enumerator = overrideCabal super.attoparsec-enumerator (drv: {
      version = "0.3.4";
      sha256 = "127mj0v6342mzxnc73qki3k197vhwsff8qkf92gm5idyxdisg5dy";
    });
    websockets = overrideCabal super.websockets (drv: {
      version = "0.9.5.0";
      sha256 = "016h213sk3n662ri8ns1sswcnaml61qckprdgxdp0m2a77amivmy";
    });
    websockets-snap = overrideCabal super.websockets-snap (drv: {
      version = "0.9.2.0";
      sha256 = "03szycdvygw1zkv2s090wn2jii9sqbplgbycmpm5mfm3r0jhbhxp";
    });
    heist = overrideCabal super.heist (drv: {
      jailbreak = true;
    });
    snap-core = overrideCabal super.snap-core (drv: {
      revision = "1";
      editedCabalFile = "1930x1w1xlyqfpwjhr64z2y12idfaz17jdk9fn699pxvb08djb85";
      jailbreak = true;
    });
    snap-server = overrideCabal super.snap-server (drv: {
      version = "0.9.5.1";
      sha256 = "18ryin6f315picrs2159sn2668266l3xchs7jb8isw0gp52273xg";
      revision = "1";
      editedCabalFile = "0p5apya7gd8kbkknpzamvnc902jdlp8kdmwrqzrj6gvxkr9ss2br";
    });
    timezone-series = overrideCabal super.timezone-series (drv: {
      jailbreak = true; # To allow time >= 1.5
    });
    timezone-olson = overrideCabal super.timezone-olson (drv: {
      jailbreak = true; # To allow time >= 1.5
    });
    "HList" = self.callPackage
      ({ mkDerivation, base, cmdargs, diffutils, directory, doctest
       , filepath, ghc-prim, hspec, lens, mtl, process, syb, tagged
       , template-haskell, profunctors
       }:
       mkDerivation {
         pname = "HList";
         version = "0.4.0.0";
         sha256 = "0f6d97vfxlml4dp6zfk95kk4la8xr5m91hiw4zj98kvwvvhb99mz";
         buildDepends = [ base ghc-prim mtl tagged template-haskell profunctors ];
         doCheck = false;
         testDepends = [
           base cmdargs directory doctest filepath hspec lens mtl process syb
         ];
         buildTools = [ diffutils ];
         jailbreak = true;
         description = "Heterogeneous lists";
         license = stdenv.lib.licenses.mit;
       }) { inherit (pkgs) diffutils;};
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
        file-embed
      ];
    });
    focus-js = self.mkDerivation ({
      license = null;
      pname = "focus-js";
      version = "0.1";
      src = ./js;
      buildDepends = with self; [ focus-core reflex reflex-dom aeson attoparsec text time vector ghcjs-dom constraints timezone-series timezone-olson ];
    });
    dependent-sum-template = overrideCabal super.dependent-sum-template (drv: {
      version = "0.0.0.4";
      src = pkgs.fetchgit {
        url = git://github.com/ryantrinkle/dependent-sum-template;
        rev = "abcd0f01a3e264e5bc1f3b00f3d03082f091ec49";
        sha256 = "16f95348c559394a39848394a9e1aa8318c79bfc62bc6946edad9aabd20a8e2d";
      };
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
    lifted-async = overrideCabal super.lifted-async (drv: {
      version = "0.7.0.1";
      sha256 = "0skfpgqlxni3bdn7pdg2732xkijmwsz655962wrbmflh987ms8y3";
    });
    JuicyPixels = overrideCabal super.JuicyPixels (drv: {
      jailbreak = true;
    });
    diagrams-svg = overrideCabal super.diagrams-svg (drv: {
      version = "1.3.1.4";
      src = pkgs.fetchgit {
        url = git://github.com/ryantrinkle/diagrams-svg;
        rev = "1abe6f74e9111b1c7efffed7fa693feacc7b6029";
        sha256 = "f02fddb399989fe35ad495602f615b7c412eea5b6a7e3cf4baeb16d2a5f548ef";
      };
    });
    lucid-svg = overrideCabal super.lucid-svg (drv: {
      version = "0.5.0.0";
      sha256 = "1p7ipdy0nmqfg1b038a1b5nd3xh2779d2gnw4h683mm5jcbf0mvj";
    });
  };

  backendHaskellPackagesBase = tryReflex.ghc;
  frontendHaskellPackagesBase = tryReflex.ghcjs;
  extendFrontendHaskellPackages = haskellPackages: haskellPackages.override {
    overrides = self: super: sharedOverrides self super // {
      stripe = self.mkDerivation ({
        pname = "stripe";
        license = null;
        src = ./hs-stripe;
        buildDepends = with self; [ aeson http-conduit http-types mtl text unordered-containers utf8-string ghcjs-base ghcjs-dom reflex-dom ];
        version = "0.8.3";
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
        license = pkgs.stdenv.lib.licenses.bsd3;
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
        src = ./groundhog/groundhog;
#        sha256 = "0n5c501wfyqcl1iy4017yyxp95kz7mb4lgc0mjjk9si36ixkww9r";
        buildDepends = with self; [
          aeson attoparsec base64-bytestring blaze-builder monad-control
          monad-logger mtl scientific text time transformers transformers-base
        ];
        homepage = "http://github.com/lykahb/groundhog";
        description = "Type-safe datatype-database mapping library";
        license = pkgs.stdenv.lib.licenses.bsd3;
        platforms = self.ghc.meta.platforms;
      });
      groundhog-th = overrideCabal super.groundhog-th (drv: {
        src = ./groundhog/groundhog-th;
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
        license = pkgs.stdenv.lib.licenses.bsd3;
        platforms = self.ghc.meta.platforms;
      });
      /*
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
        jailbreak = true;
      });
      */
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
          tar
          file-embed
          binary
          lucid
          diagrams
          diagrams-svg
          raw-strings-qq
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
        license = pkgs.stdenv.lib.licenses.bsd3;
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
  mkPreConfigure = haskellPackages: pname: executableName: depends: ''
    if ! ls | grep ".*\\.cabal$" ; then
      cat >"${pname}.cabal" <<EOF
    name: ${pname}
    version: ${appVersion}
    cabal-version: >= 1.2
    ${if executableName != null then executableHeader executableName else libraryHeader}
      hs-source-dirs: src
      build-depends: ${pkgs.lib.concatStringsSep "," ([ "base" "bytestring" "containers" "time" "transformers" "text" "lens" "aeson" "mtl" "directory" "deepseq" "binary" "async" ] ++ (if haskellPackages.ghc.isGhcjs or false then [ "ghcjs-base" ] else []) ++ builtins.filter (x: x != null) (builtins.map (x: x.pname or null) depends))}
      other-extensions: TemplateHaskell
      ghc-options: -threaded -Wall -fwarn-tabs -funbox-strict-fields -O2 -fprof-auto-calls -rtsopts -threaded -with-rtsopts=-N10
    EOF
    fi
  '';

  common = haskellPackages: haskellPackages.mkDerivation (rec {
    pname = "${appName}-common";
    version = appVersion;
    src = ../common;
    license = null;
    preConfigure = mkPreConfigure haskellPackages pname null buildDepends;
    buildDepends = with haskellPackages; [
      focus-core
    ] ++ commonDepends haskellPackages;
    buildTools = [] ++ commonTools pkgs;
  });
  mkFrontend = src: haskellPackages:
    with haskellPackages;
    let
      frontendCommon = common haskellPackages;
      in haskellPackages.mkDerivation (rec {
        pname = "${appName}-frontend";
        version = appVersion;
        license = null;
        inherit src;
        preConfigure = mkPreConfigure haskellPackages pname "frontend" buildDepends;
        buildDepends = [
          frontendCommon
          focus-core
          focus-js
          ghcjs-dom
        ] ++ frontendDepends haskellPackages;
        buildTools = [] ++ frontendTools pkgs;
        isExecutable = true;
        passthru = {
          common = frontendCommon;
          inherit haskellPackages;
        };
    });
  mkGhcjsApp = src: pkgs.stdenv.mkDerivation (rec {
    name = "ghcjs-app";
    unminified = mkFrontend src frontendHaskellPackages;
    builder = builtins.toFile "builder.sh" ''
      source "$stdenv/setup"

      mkdir -p "$out/frontend.jsexe"
      closure-compiler -O ADVANCED --js_output_file="$out/frontend.jsexe/all.js" "$unminified/bin/frontend.jsexe/all.js"
    '';
    buildInputs = with pkgs; [
      closurecompiler
    ];
    passthru = {
      frontend = unminified;
    };
  });
  # Takes an asset, compresses it for various
  compress = file: pkgs.stdenv.mkDerivation {
    input = file;
    builder = builtins.toFile "builder.sh" ''
      source "$stdenv/setup"

      mkdir -p "$out"

      cp "$input" "$out/identity" & #TODO: Test this
      zopfli -c --i5 --gzip "$input" >"$out/gzip" & #TODO: Test this
      zopfli -c --i5 --zlib "$input" >"$out/compress" & #TODO: Test this
      zopfli -c --i5 --deflate "$input" >"$out/deflate" & #TODO: Test this

      wait
    '';
    buildInputs = with pkgs; [
      zopfli
    ];
  };
in pkgs.stdenv.mkDerivation (rec {
  name = "${appName}-${appVersion}";
  static = ../static;
  marketing = ../marketing;
  # Give the minification step its own derivation so that backend rebuilds don't redo the minification
  frontend = mkGhcjsApp ../frontend;
  builder = builtins.toFile "builder.sh" ''
    source "$stdenv/setup"

    mkdir -p "$out"
    cp -r "$static" "$out/static"
    cp -r "$marketing" "$out/marketing"
    ln -s "$backend/bin/backend" "$out"
    ln -s "$frontend/frontend.jsexe" "$out"
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
      preConfigure = mkPreConfigure backendHaskellPackages pname "backend" buildDepends;
      preBuild = ''
        ln -sfT ${static} static
      '';
      buildDepends = [
        backendCommon
        vector-algorithms
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
  passthru = {
    frontend = frontend.unminified;
    frontendGhc = mkFrontend ../frontend backendHaskellPackages;
  };
})
