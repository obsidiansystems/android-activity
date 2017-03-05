{ nixpkgs, filterGitSource }:

with nixpkgs.haskell.lib;
let inherit (nixpkgs) stdenv;
in
self: super: {
    reflex = enableCabalFlag super.reflex "specialize-to-spidertimeline-global";
    reflex-dom-core = dontCheck super.reflex-dom-core;
    attoparsec-enumerator = overrideCabal super.attoparsec-enumerator (drv: {
      version = "0.3.4";
      sha256 = "127mj0v6342mzxnc73qki3k197vhwsff8qkf92gm5idyxdisg5dy";
    });
    clay = overrideCabal super.clay (drv: {
      version = "0.11";
      sha256 = "1hdk551wl5hg9danz4yf4h7bs8w3076s6hyg6h3fq20mn9hj65y3";
      revision = null;
      editedCabalFile = null;
    });
    snap = self.callPackage ({ mkDerivation, aeson, async, attoparsec, base, bytestring, cereal
           , clientsession, configurator, containers, deepseq, directory
           , directory-tree, dlist, filepath, Glob, hashable, heist
           , http-streams, HUnit, lens, lifted-base, map-syntax, monad-control
           , mtl, mwc-random, pwstore-fast, QuickCheck, smallcheck, snap-core
           , snap-server, stdenv, stm, syb, test-framework
           , test-framework-hunit, test-framework-quickcheck2
           , test-framework-smallcheck, text, time, transformers
           , transformers-base, unordered-containers, xmlhtml
           }:
    mkDerivation {
      pname = "snap";
      version = "1.0.0.1";
      sha256 = "1f1d69900rdvsi323cc23pfgacmvgclbdaw57lfi54s7830icgr9";
      libraryHaskellDepends = [
        aeson attoparsec base bytestring cereal clientsession configurator
        containers directory directory-tree dlist filepath hashable heist
        lens lifted-base map-syntax monad-control mtl mwc-random
        pwstore-fast snap-core snap-server stm text time transformers
        transformers-base unordered-containers xmlhtml
      ];
      testHaskellDepends = [
        aeson async attoparsec base bytestring cereal clientsession
        configurator containers deepseq directory directory-tree dlist
        filepath Glob hashable heist http-streams HUnit lens lifted-base
        map-syntax monad-control mtl mwc-random pwstore-fast QuickCheck
        smallcheck snap-core snap-server stm syb test-framework
        test-framework-hunit test-framework-quickcheck2
        test-framework-smallcheck text time transformers transformers-base
        unordered-containers xmlhtml
      ];
      homepage = "http://snapframework.com/";
      description = "Top-level package for the Snap Web Framework";
      license = stdenv.lib.licenses.bsd3;
      jailbreak = true;
    }) {};

    snap-core = dontCheck (self.callPackage ({ mkDerivation, attoparsec, base, bytestring, bytestring-builder
                                  , case-insensitive, containers, deepseq, directory, filepath
                                  , hashable, HUnit, io-streams, lifted-base, monad-control, mtl
                                  , old-locale, parallel, QuickCheck, random, readable, regex-posix
                                  , stdenv, test-framework, test-framework-hunit
                                  , test-framework-quickcheck2, text, time, transformers
                                  , transformers-base, unix-compat, unordered-containers, vector
                                  , zlib
                                  }:
    mkDerivation {
      pname = "snap-core";
      version = "1.0.0.0";
      sha256 = "1nqak3z4crvg24s02jxrqi5hnp34wj655mq5ysfvxsqdzibscdcw";
      libraryHaskellDepends = [
        attoparsec base bytestring bytestring-builder case-insensitive
        containers directory filepath HUnit io-streams lifted-base
        monad-control mtl old-locale random readable regex-posix text time
        transformers transformers-base unix-compat unordered-containers
        vector
      ];
      testHaskellDepends = [
        attoparsec base bytestring bytestring-builder case-insensitive
        containers deepseq directory filepath hashable HUnit io-streams
        lifted-base monad-control mtl old-locale parallel QuickCheck random
        readable regex-posix test-framework test-framework-hunit
        test-framework-quickcheck2 text time transformers transformers-base
        unix-compat unordered-containers vector zlib
      ];
      homepage = "http://snapframework.com/";
      description = "Snap: A Haskell Web Framework (core interfaces and types)";
      license = stdenv.lib.licenses.bsd3;
    }) {});

  "snap-server" = self.callPackage
    ({ mkDerivation, attoparsec, base, base16-bytestring, blaze-builder
     , bytestring, bytestring-builder, case-insensitive, clock
     , containers, deepseq, directory, filepath, HsOpenSSL, http-common
     , http-streams, HUnit, io-streams, io-streams-haproxy, lifted-base
     , monad-control, mtl, network, old-locale, openssl-streams
     , parallel, QuickCheck, random, snap-core, test-framework
     , test-framework-hunit, test-framework-quickcheck2, text, threads
     , time, transformers, unix, unix-compat, vector
     }:
     mkDerivation {
       pname = "snap-server";
       version = "1.0.1.0";
       sha256 = "a398b15e90d2d6bc77af3edf6f5926df7863073a4774c71a46c0adb43c837488";
       configureFlags = [ "-fopenssl" ];
       libraryHaskellDepends = [
         attoparsec base blaze-builder bytestring bytestring-builder
         case-insensitive clock containers filepath HsOpenSSL io-streams
         io-streams-haproxy lifted-base mtl network old-locale
         openssl-streams snap-core text time unix unix-compat vector
       ];
       testHaskellDepends = [
         attoparsec base base16-bytestring blaze-builder bytestring
         bytestring-builder case-insensitive clock containers deepseq
         directory filepath HsOpenSSL http-common http-streams HUnit
         io-streams io-streams-haproxy lifted-base monad-control mtl network
         old-locale openssl-streams parallel QuickCheck random snap-core
         test-framework test-framework-hunit test-framework-quickcheck2 text
         threads time transformers unix unix-compat vector
       ];
       homepage = "http://snapframework.com/";
       description = "A web server for the Snap Framework";
       license = stdenv.lib.licenses.bsd3;
       hydraPlatforms = stdenv.lib.platforms.none;
       jailbreak = true;
     }) {};

    io-streams-haproxy = self.callPackage (
      { mkDerivation, attoparsec, base, bytestring, HUnit, io-streams
      , network, stdenv, test-framework, test-framework-hunit
      , transformers
      }:
      mkDerivation {
        pname = "io-streams-haproxy";
        version = "1.0.0.1";
        sha256 = "0zwjdsg1pcxzd8s0d308q4jhx0pfrk2aq8q039gs8k9y8h9cbh64";
        libraryHaskellDepends = [
          attoparsec base bytestring io-streams network transformers
        ];
        testHaskellDepends = [
          attoparsec base bytestring HUnit io-streams network test-framework
          test-framework-hunit transformers
        ];
        homepage = "http://snapframework.com/";
        description = "HAProxy protocol 1.5 support for io-streams";
        license = stdenv.lib.licenses.bsd3;
      }) {};
    heist = dontCheck (self.callPackage (
      { mkDerivation, aeson, attoparsec, base, bifunctors, blaze-builder
      , blaze-html, bytestring, containers, criterion, directory
      , directory-tree, dlist, errors, filepath, hashable, HUnit, lens
      , lifted-base, map-syntax, monad-control, mtl, process, QuickCheck
      , random, statistics, stdenv, test-framework, test-framework-hunit
      , test-framework-quickcheck2, text, time, transformers
      , transformers-base, unordered-containers, vector, xmlhtml
      }:
      mkDerivation {
        pname = "heist";
        version = "1.0.0.0";
        sha256 = "0kzc1wb0v481nrn7c1ygkh8ph9g19pm2knbgpwbcg3a86pl7wqvc";
        revision = "2";
        editedCabalFile = "6da6ec8736d1cb25caa9ca7b82e0eab1ebd2c99804fa7ac23b933589f550a637";
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson attoparsec base blaze-builder blaze-html bytestring
          containers directory directory-tree dlist filepath hashable
          lifted-base map-syntax monad-control mtl process random text time
          transformers transformers-base unordered-containers vector xmlhtml
        ];
        executableHaskellDepends = [
          aeson attoparsec base blaze-builder blaze-html bytestring
          containers criterion directory directory-tree dlist errors filepath
          hashable HUnit lifted-base map-syntax monad-control mtl process
          random statistics test-framework test-framework-hunit text time
          transformers transformers-base unordered-containers vector xmlhtml
        ];
        testHaskellDepends = [
          aeson attoparsec base bifunctors blaze-builder blaze-html
          bytestring containers directory directory-tree dlist errors
          filepath hashable HUnit lens lifted-base map-syntax monad-control
          mtl process QuickCheck random test-framework test-framework-hunit
          test-framework-quickcheck2 text time transformers transformers-base
          unordered-containers vector xmlhtml
        ];
        homepage = "http://snapframework.com/";
        description = "An Haskell template system supporting both HTML5 and XML";
        license = stdenv.lib.licenses.bsd3;
        jailbreak = true;
      }) {});
    xmlhtml = self.callPackage (
      { mkDerivation, base, blaze-builder, blaze-html, blaze-markup
      , bytestring, containers, directory, HUnit, parsec, QuickCheck
      , stdenv, test-framework, test-framework-hunit
      , test-framework-quickcheck2, text, unordered-containers
      }:
      mkDerivation {
        pname = "xmlhtml";
        version = "0.2.3.5";
        sha256 = "0vdhfh1fnhmkymasrcv5rh4498r5fgm7yia3n5h8n1nmmz3s2cz3";
        revision = "1";
        editedCabalFile = "6a4d1fc061c4dd01628d762d947e63619a25714aa0dd36b6fe674a7ec62b9045";
        libraryHaskellDepends = [
          base blaze-builder blaze-html blaze-markup bytestring containers
          parsec text unordered-containers
        ];
        testHaskellDepends = [
          base blaze-builder blaze-html blaze-markup bytestring containers
          directory HUnit parsec QuickCheck test-framework
          test-framework-hunit test-framework-quickcheck2 text
          unordered-containers
        ];
        homepage = "https://github.com/snapframework/xmlhtml";
        description = "XML parser and renderer with HTML 5 quirks mode";
        license = stdenv.lib.licenses.bsd3;
        jailbreak = true;
      }) {};
    mustache = overrideCabal super.mustache (drv: {
      doCheck = false;
    });
    groundhog = self.mkDerivation ({
      pname = "groundhog";
      version = "0.7.0.3";
      src = filterGitSource ./groundhog/groundhog;
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
      src = filterGitSource ./groundhog/groundhog-th;
      sha256 = null;
      revision = null;
      editedCabalFile = null;
    });
    groundhog-postgresql = overrideCabal super.groundhog-postgresql (drv: {
      src = filterGitSource ./groundhog/groundhog-postgresql;
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
      src = filterGitSource ./hs-stripe;
      buildDepends = with self; [ aeson http-conduit http-types mtl text unordered-containers utf8-string ghcjs-dom reflex-dom ];
      version = "0.8.3";
    });
    stripe-tests = self.callPackage ./stripe/stripe-tests/stripe-tests.nix {};
    stripe-core = self.callPackage ./stripe/stripe-core/stripe-core.nix {};
    stripe-http-streams = self.callPackage ./stripe/stripe-http-streams/stripe-http-streams.nix {};
    stripe-haskell = self.callPackage ./stripe/stripe-haskell/stripe-haskell.nix {};
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
      src = filterGitSource ./amazonka/amazonka;
    });
    amazonka-core = overrideCabal super.amazonka-core (drv: {
      version = "0.3.4";
      src = filterGitSource ./amazonka/core;
    });
    amazonka-ec2 = overrideCabal super.amazonka-ec2 (drv: {
      version = "0.3.4";
      src = filterGitSource ./amazonka/amazonka-ec2;
    });
    amazonka-s3 = overrideCabal super.amazonka-s3 (drv: {
      version = "0.3.4";
      src = filterGitSource ./amazonka/amazonka-s3;
    });
    amazonka-route53 = overrideCabal super.amazonka-route53 (drv: {
      version = "0.3.4";
      src = filterGitSource ./amazonka/amazonka-route53;
    });
    amazonka-cloudwatch = overrideCabal super.amazonka-cloudwatch (drv: {
      version = "0.3.4";
      src = filterGitSource ./amazonka/amazonka-cloudwatch;
    });
    amazonka-iam = overrideCabal super.amazonka-iam (drv: {
      version = "0.3.4";
      src = filterGitSource ./amazonka/amazonka-iam;
    });
    amazonka-sts = overrideCabal super.amazonka-sts (drv: {
      version = "0.3.4";
      src = filterGitSource ./amazonka/amazonka-sts;
    });
    JuicyPixels = overrideCabal super.JuicyPixels (drv: {
      jailbreak = true;
    });
    rex = overrideCabal super.rex (drv: {
      src = nixpkgs.fetchFromGitHub {
        owner = "ali-abrar";
        repo = "rex";
        rev = "b575bcafa4853752b6490e95502f426431a7b213";
        sha256 = "0gxpsaf1gwd24frrlfkf7arjpylv11x2vkrp9qwi85l2gq35di2x";
      };
    });
    diagrams-svg = overrideCabal super.diagrams-svg (drv: {
      version = "1.3.1.10";
      src = nixpkgs.fetchFromGitHub {
        owner = "diagrams";
        repo = "diagrams-svg";
        rev = "0fcfe833844baccd567bc01986ffe0462f2c2d18";
        sha256 = "1mwibd972n1xv9ywf4jidmfc7w9qbv5xy10afgammn71ziniz29y";
      };
    });
    # disable tests so that it doesn't build gpx-conduit
    gps = dontCheck super.gps;
    # gpx-conduit = overrideCabal super.gpx-conduit (drv: {
    #   src = nixpkgs.fetchgit {
    #     url = git://github.com/obsidiansystems/gpx-conduit;
    #     rev = "988d97c433042e5af46ae7c38e1163eecbf928bb";
    #     sha256 = "9cd3d6b3c27f9a0ae5c02769001d56f773efab9657d50b948d4355397950f28c";
    #   };
    # });
    map-syntax = doJailbreak super.map-syntax;
    zlib-enum = doJailbreak super.zlib-enum;
    reflex-jsx = self.callPackage ./reflex-jsx.nix {};
    pontarius-xmpp =
      let p = overrideCabal super.pontarius-xmpp (drv: {
                src = nixpkgs.fetchgit {
                  url = git://github.com/pontarius/pontarius-xmpp;
                  rev = "446f11ea70b3921ad44fee6d532135fb9b775d7d";
                  sha256 = "0bjv0s1s5916d37270xhv0lfixf60n691wn7bin37pvm5hxs51xj";
                };
              });
      in dontCheck p;
    postie = overrideCabal super.postie (drv: {
      version = "0.5.0.1";
      src = filterGitSource ./postie;
    });
    mime-mail = overrideCabal super.mime-mail (drv: {
      src = "${nixpkgs.fetchgit {
        url = git://github.com/obsidiansystems/mime-mail;
        rev = "53f68271530a51d6709d8e5e3c88fbe67e8de837";
        sha256 = "12l6d1cy658bai70yr4qyd8sla9xnln9w7fd3abbinmsqssjfllc";
      }}/mime-mail";
    });
    hsemail = overrideCabal super.hsemail (drv: {
      version = "1.7.8";
      src = filterGitSource ./hsemail;
    });
    imagemagick = super.imagemagick.override {
      imagemagick = nixpkgs.imagemagickBig; # Necessary for PDF support
    };
    hdevtools = overrideCabal super.hdevtools (drv: {
      version = "0.1.5.0";
      sha256 = "1rq41mlvfsjqf21hnv5jslnfk39z52p0a6a0yk2ng8q971pj70gd";
    });
    smtp-mail = overrideCabal super.smtp-mail (drv: {
      src = nixpkgs.fetchFromGitHub {
        owner = "obsidiansystems";
        repo = "smtp-mail";
        rev = "5e6f78e4eb073c6e167daac7f50cc00a04c65d4e";
        sha256 = "0c2xcgigxgjlwci594s3p1ydy0g3c1c1wjblrr6d08iibvlh23y0";
      };
    });
    webdriver = overrideCabal super.webdriver (drv: {
      src = nixpkgs.fetchFromGitHub {
        owner = "obsidiansystems";
        repo = "hs-webdriver";
        rev = "e9d2c7a7087ca3a88e517dafc6d2de4323fb177e";
        sha256 = "0vsvb5k9k42r5hpdqr87qy0yahqapyldzmdqr5nxpkyyddsn2bfs";
      };
    });
    diagrams-lib = overrideCabal super.diagrams-lib (drv: {
      jailbreak = true;
    });
    th-expand-syns = overrideCabal super.th-expand-syns (drv: {
      jailbreak = true;
    });
    focus-heremaps = self.mkDerivation ({
      pname = "focus-heremaps";
      version = "0.1";
      src = filterGitSource ./heremaps;
      license = nixpkgs.stdenv.lib.licenses.bsd3;
      buildDepends = with self; [
        focus-core aeson text http-conduit time
      ];
    });
    focus-google-maps = self.mkDerivation ({
      pname = "focus-google-maps";
      version = "0.1";
      src = filterGitSource ./google-maps;
      license = nixpkgs.stdenv.lib.licenses.bsd3;
      buildDepends = with self; [
        bifunctors data-default focus-js ghcjs-dom lens jsaddle reflex reflex-dom-core these text mtl containers
      ];
    });
  }
