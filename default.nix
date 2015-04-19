/*
let overrideCabal = drv: f: (drv.override (args: args // {
      mkDerivation = drv: args.mkDerivation (drv // f drv);
    })) // {
      overrideScope = scope: overrideCabal (drv.overrideScope scope) f;
    };
    nixpkgs = import ./nixpkgs { config.allowUnfree = true; };
    extendHaskellPackages = haskellPackages: haskellPackages.override {
      overrides = self: super: {
        reflex = self.callPackage ./reflex {};
        reflex-dom = self.callPackage ./reflex-dom {};
      };
    };
in rec {
  inherit nixpkgs;
  ghc = extendHaskellPackages nixpkgs.pkgs.haskell-ng.packages.ghc7101;
  ghcjs = extendHaskellPackages nixpkgs.pkgs.haskell-ng.packages.ghcjs;
  platforms = [ "ghcjs" ] ++ (if !nixpkgs.stdenv.isDarwin then [ "ghc" ] else []);
}
*/
let
  enableProfiling = false;
  pkgs = import ./nixpkgs { config.allowUnfree = true; };
  backendHaskellPackagesBase = if enableProfiling then pkgs.haskell-ng.packages.ghc784 else pkgs.haskell-ng.packages.ghc784;
  overrideCabal = drv: f: (drv.override (args: args // {
    mkDerivation = drv: args.mkDerivation (drv // f drv);
  })) // {
    overrideScope = scope: overrideCabal (drv.overrideScope scope) f;
  };
  /*
  extendHaskellPackages = haskellPackages: haskellPackages.override {
    overrides = self: super: {
      reflex = self.callPackage ./reflex {};
      reflex-dom = self.callPackage ./reflex-dom {};
    };
  };
  */

  extendBackendHaskellPackages = haskellPackages: haskellPackages.override {
    overrides = self: super: {
      snap = self.mkDerivation ({
        pname = "snap";
        version = "0.14.0.2";
        /* revision = "1"; */
        sha256 = "1yv1snkibsqd7cdxyqi7c8gvnv1hzzhw5jlk19kps526n5xvay7r";
        editedCabalFile = "1640756ec7bfd3130869dce451904d6cc762ab6c8b8128982933fba80f325c92";
        isLibrary = true;
        isExecutable = true;
        buildDepends = with haskellPackages; [
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
      focus = self.mkDerivation ({
        pname = "focus";
        license = null;
        version = "0.1";
        src = ./core;
        buildDepends = with self; [ aeson attoparsec text time vector ];
      });
      stripe = self.mkDerivation ({
        pname = "stripe";
        license = null;
        src = ./hs-stripe;
        buildDepends = with self; [ aeson http-conduit http-types mtl text unordered-containers utf8-string ];
        version = "0.8.3";
      });
    };
  };
  backendHaskellPackages = extendBackendHaskellPackages backendHaskellPackagesBase;
  backendCabal = backendHaskellPackagesBase.Cabal_1_22_0_0;
in {name, version}:
let
  # Break recursion
  appName = name;
  appVersion = version;

  libraryHeader = ''
    library
      exposed-modules: $(cd src ; find * -iname '*.hs' | sed 's/\.hs$//' | tr / . | tr "\n" , | sed 's/,$//')
  '';
  executableHeader = executableName: ''
    executable ${executableName}
      main-is: $(cd src; ls | grep -i '^\(${executableName}\|main\)\.\(l\|\)hs'$)
  '';
  mkPreConfigure = pname: ghcPkgName: executableName: ''
    if ! ls | grep ".*\\.cabal$" ; then
      cat >"${pname}.cabal" <<EOF
    name: ${pname}
    version: ${appVersion}
    cabal-version: >= 1.2
    ${if executableName != null then executableHeader executableName else libraryHeader}
      hs-source-dirs: src
      build-depends: $(${ghcPkgName} list --global | cat | sed -n 's/^    \([^(].*\)-[0-9.]*$/\1/p' | grep -v 'bin-package-db\|haskeline\|terminfo' | tr '\n' , | sed 's/,$//')
      other-extensions: TemplateHaskell
      ghc-options: -threaded -Wall -fwarn-tabs -funbox-strict-fields -O2 -fprof-auto-calls -rtsopts
    EOF
    fi
  '';

  common = haskellPackages: cabal: ghcPkgName: haskellPackages.cabal.mkDerivation (self: rec {
    pname = "${appName}-common";
    version = appVersion;
    src = ../common;
    preConfigure = mkPreConfigure pname ghcPkgName null;
    buildDepends = with haskellPackages; [
      mtl
      text
      time
      lens
      aeson
      transformers
      timezoneSeries
      timezoneOlson
      focus
      network
      networkUri
      semigroups
      stripe
    ];
  });
in pkgs.stdenv.mkDerivation (rec {
  name = "${appName}-${appVersion}";
  static = ../static;
  marketing = ../marketing;
  builder = builtins.toFile "builder.sh" ''
    source $stdenv/setup

    mkdir -p $out
    cp -r $static $out/static
    cp -r $marketing $out/marketing
    ln -s $backend/bin/backend $out
    ln -st $out $frontend/bin/*
  '';
  backend =
    with backendHaskellPackages;
    let
      focusBackend = backendHaskellPackages.mkDerivation ({
        pname = "focus-backend";
        license = null;
        version = "0.1";
        src = ./backend;
        buildDepends = [ groundhog mtl focus lens aeson snap resource-pool text network stm postgresql-simple groundhog-postgresql websockets-snap websockets stripe ];
      });
      ghcPkgName = "ghc-pkg";
      /*myCommon = common backendHaskellPackages backendCabal ghcPkgName;*/
    in backendHaskellPackages.mkDerivation (rec {
      pname = "${appName}-backend";
      license = null;
      version = appVersion;
      src = ../backend;
      /*common = myCommon;*/
      preConfigure = mkPreConfigure pname ghcPkgName "backend";
      preBuild = ''
        ln -sf ${pkgs.tzdata}/share/zoneinfo .
      '';
      buildDepends = [
        backendCabal
        template-haskell focusBackend MonadCatchIO-transformers mtl snap snap-core snap-server snap-loader-static text time lens postgresql-simple resource-pool aeson attoparsec vector tagged derive dependent-sum dependent-map MemoTrie transformers monad-loops vector-space yaml websockets-snap MaybeT clientsession smtp-mail blaze-html timezone-series timezone-olson file-embed these groundhog groundhog-th groundhog-postgresql focus filepath http-client singletons
      ];
      isExecutable = true;
      isLibrary = false;
      jailbreak = true;
      configureFlags = [ "--ghc-option=-lgcc_s" ] ++ (if enableProfiling then [ "--enable-executable-profiling" ] else [ ]);
    });
  frontend =
    let haskellPackages = pkgs.haskellPackages_ghcjs.override {
          extension = self: super: with self; {
            network = self.network_2_6_0_2;
            reflex = self.callPackage ./reflex {};
            reflexDom = self.callPackage ./reflex-dom {};
            stripe = self.callPackage ./hs-stripe {};
            focus = cabal.mkDerivation (self: {
              pname = "focus-core";
              version = "0.1";
              src = ./core;
              buildDepends = [ aeson attoparsec text time vector ];
            });
            focusJs = cabal.mkDerivation (self: {
              pname = "focus-js";
              version = "0.1";
              src = ./js;
              buildDepends = [ focus reflex reflexDom aeson attoparsec text time vector ghcjsBase ghcjsDom ];
            });
          };
        };
        ghcPkgName = "ghcjs-pkg";
        myCommon = common haskellPackages haskellPackages.cabal ghcPkgName;
    in with haskellPackages; cabal.mkDerivation (self: rec {
      pname = "${appName}-frontend";
      version = appVersion;
      src = ../frontend;
      common = myCommon;
      preConfigure = mkPreConfigure pname ghcPkgName "frontend";
      buildDepends = [
        myCommon
        pkgs.nodejs time mtl text aeson attoparsec split lens vector semigroups derive dependentSum dependentMap MemoTrie transformers monadLoops vectorSpace haskellSrcExts safe timezoneOlson timezoneSeries these network ghcjsDom reflex reflexDom focus focusJs fileEmbed randomFu MonadRandom stripe
        httpTypes # For oauth-netDocuments
      ];
      buildTools = [ ghc.ghc.parent.cabalInstall ];
    });
})
