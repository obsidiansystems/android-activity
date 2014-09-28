let
  enableProfiling = false;
  pkgs = import ./nixpkgs {};
  backendHaskellPackagesBase = if enableProfiling then pkgs.haskellPackages_ghc763_profiling else pkgs.haskellPackages_ghc763;
  backendHaskellPackages = backendHaskellPackagesBase.override {
    extension = self: super: {
      snapServer = super.snapServer.override {
        cabal = self.cabal.override {
          extension = self: super: {
            src = pkgs.fetchgit {
              url = https://github.com/ryantrinkle/snap-server.git;
              rev = "72e180d6324244ac65773872d42f25d4bcc083a4";
              sha256 = "ed6e7155d9c9f9ede2b13bc8c1eacdbff4b83b0f9bb42de2c8f0d1275aeab406";
            };
          };
        };
      };
      focus = self.cabal.mkDerivation (selfInner: {
        pname = "focus";
        version = "0.1";
        src = ./core;
        buildDepends = with self; [ aeson attoparsec text time vector ];
      });
    };
  };
  backendCabal = backendHaskellPackages.cabal.override { Cabal = backendHaskellPackages.Cabal_1_18_1_3; };
in {name, version}:
let
  # Break recursion
  appName = name;
  appVersion = version;

  common = haskellPackages: cabal: haskellPackages.cabal.mkDerivation (self: {
    pname = "${appName}-common";
    version = appVersion;
    src = ../common;
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
      semigroups
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
    ln -s $frontend/bin/frontend.jsexe $out
  '';
  backend =
    with backendHaskellPackages;
    let
      groundhog = cabal.mkDerivation (self: {
        pname = "groundhog";
        version = "0.4.2.2";
        sha256 = "0b5bsz62zhqb281xrd8ak7iv4dp9am1cn15lxwynsmcgwa3ahhp6";
        buildDepends = [
          blazeBuilder monadControl monadLogger mtl text time transformers
          transformersBase
        ];
      });
      groundhogTh = cabal.mkDerivation (self: {
        pname = "groundhog-th";
        version = "0.4.2.2";
        sha256 = "1b0vlmp9n1qh472lybk3cc7prxmx1561bhf44w46jkl897qji0lx";
        buildDepends = [ groundhog text time yaml ];
      });
      groundhogPostgresql = cabal.mkDerivation (self: {
        pname = "groundhog-postgresql";
        version = "0.4.2.2";
        sha256 = "1d3gni5lld84m3irz9pdndixghkf3cmi7l3mcvh46xk76grg3bza";
        buildDepends = [
          attoparsec blazeBuilder groundhog monadControl monadLogger
          postgresqlLibpq postgresqlSimple resourcePool text time
          transformers
        ];
      });
    in backendCabal.mkDerivation (self: {
      pname = "${appName}-backend";
      version = appVersion;
      src = ../backend;
      preBuild = ''
        ln -sf ${pkgs.tzdata}/share/zoneinfo .
      '';
      buildDepends = [
        (common backendHaskellPackages backendCabal)
        MonadCatchIOTransformers mtl snap snapCore snapServer snapLoaderStatic text time lens postgresqlSimple resourcePool aeson attoparsec vector tagged derive dependentSum dependentMap MemoTrie transformers monadLoops vectorSpace yaml websocketsSnap MaybeT clientsession smtpMail blazeHtml timezoneSeries timezoneOlson fileEmbed these groundhog groundhogTh groundhogPostgresql focus filepath
      ];
      jailbreak = true;
      configureFlags = [ "--ghc-option=-lgcc_s" ] ++ (if enableProfiling then [ "--enable-executable-profiling" ] else [ ]);
    });
  frontend =
    let haskellPackages = pkgs.haskellPackages_ghcjs.override {
          extension = self: super: with self; {
            reflex = cabal.mkDerivation (self: {
              pname = "reflex";
              version = "0.1";
              src = ./reflex;
              buildDepends = [
                time mtl lens semigroups dependentSum dependentMap MemoTrie transformers monadLoops vectorSpace these testFramework testFrameworkQuickcheck2 strict
              ];
              jailbreak = true;
            });
            reflexDom = cabal.mkDerivation (self: {
              pname = "reflex-dom";
              version = "0.1";
              src = ./reflex-dom;
              buildDepends = [
                aeson dependentMap dependentSum ghcjsDom lens MemoTrie monadLoops mtl reflex safe semigroups text these time transformers vectorSpace reflex
              ];
              jailbreak = true;
            });
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
              buildDepends = [ aeson attoparsec text time vector ghcjsBase ghcjsDom ];
            });
          };
        };
    in with haskellPackages; cabal.mkDerivation (self: rec {
      pname = "${appName}-frontend-${appVersion}";
      version = appVersion;
      src = ../frontend;

      buildDepends = [
        (common haskellPackages cabal)
        time mtl text aeson attoparsec split lens vector semigroups derive dependentSum dependentMap MemoTrie transformers monadLoops vectorSpace haskellSrcExts safe timezoneOlson timezoneSeries these network ghcjsDom reflex reflexDom focus focusJs fileEmbed
      ];
      buildTools = [ ghc.ghc.parent.cabalInstallGhcjs ];
    });
})

