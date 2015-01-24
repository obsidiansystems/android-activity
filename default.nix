let
  enableProfiling = false;
  pkgs = import ./nixpkgs {};
  backendHaskellPackagesBase = if enableProfiling then pkgs.haskellPackages_ghc784_profiling else pkgs.haskellPackages_ghc784;
  backendHaskellPackages = backendHaskellPackagesBase.override {
    extension = self: super: {
      network = self.network_2_6_0_2;
      snapServer = super.snapServer.override {
        cabal = self.cabal.override {
          extension = self: super: {
            src = pkgs.fetchgit {
              url = https://github.com/ryantrinkle/snap-server.git;
              rev = "72e180d6324244ac65773872d42f25d4bcc083a4";
              sha256 = "ed6e7155d9c9f9ede2b13bc8c1eacdbff4b83b0f9bb42de2c8f0d1275aeab406";
            };
            jailbreak = true; # Necessary to use network-2.6.*
          };
        };
      };
      focus = self.cabal.mkDerivation (selfInner: {
        pname = "focus";
        version = "0.1";
        src = ./core;
        buildDepends = with self; [ aeson attoparsec text time vector ];
      });
      stripe = self.callPackage ./hs-stripe {};
    };
  };
  backendCabal = backendHaskellPackages.cabal;
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
      groundhog = cabal.mkDerivation (self: {
        pname = "groundhog";
        version = "0.5.1";
        sha256 = "1v6by5jxymgyxy27m853z1b7xg5gksvm42kpc5rxr3aw1qssqbn5";
        buildDepends = [
          blazeBuilder monadControl monadLogger mtl text time transformers
          transformersBase
        ];
      });
      groundhogTh = cabal.mkDerivation (self: {
        pname = "groundhog-th";
        version = "0.5.1";
        sha256 = "1pw3xd4mcivav3w43xg022byf8jgqir56hf978ly6a560bm3m8xp";
        buildDepends = [ groundhog text time yaml ];
      });
      groundhogPostgresql = cabal.mkDerivation (self: {
        pname = "groundhog-postgresql";
        version = "0.5.1";
        sha256 = "0d1dc5gscg5q3jh25sb407az107phwbv199a40pgvg5zkhlaphq8";
        buildDepends = [
          attoparsec blazeBuilder groundhog monadControl monadLogger
          postgresqlLibpq postgresqlSimple resourcePool text time
          transformers
        ];
      });
      focusBackend = cabal.mkDerivation (self: {
        pname = "focus-backend";
        version = "0.1";
        src = ./backend;
        buildDepends = [ groundhog mtl focus lens aeson snap resourcePool text network stm postgresqlSimple groundhogPostgresql websocketsSnap websockets ];
      });
      ghcPkgName = "ghc-pkg";
      myCommon = common backendHaskellPackages backendCabal ghcPkgName;
    in backendCabal.mkDerivation (self: rec {
      pname = "${appName}-backend";
      version = appVersion;
      src = ../backend;
      common = myCommon;
      preConfigure = mkPreConfigure pname ghcPkgName "backend";
      preBuild = ''
        ln -sf ${pkgs.tzdata}/share/zoneinfo .
      '';
      buildDepends = [
        myCommon
        focusBackend MonadCatchIOTransformers mtl snap snapCore snapServer snapLoaderStatic text time lens postgresqlSimple resourcePool aeson attoparsec vector tagged derive dependentSum dependentMap MemoTrie transformers monadLoops vectorSpace yaml websocketsSnap MaybeT clientsession smtpMail blazeHtml timezoneSeries timezoneOlson fileEmbed these groundhog groundhogTh groundhogPostgresql focus filepath httpClient
      ];
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
