{ nixpkgs, filterGitSource }:

with nixpkgs.haskell.lib;
let inherit (nixpkgs) stdenv;
in
self: super: {
    reflex-dom-core = dontCheck super.reflex-dom-core;
    clay = dontCheck super.clay;
    attoparsec-enumerator = overrideCabal super.attoparsec-enumerator (drv: {
      version = "0.3.4";
      sha256 = "127mj0v6342mzxnc73qki3k197vhwsff8qkf92gm5idyxdisg5dy";
    });
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
    # disable tests so that it doesn't build gpx-conduit
    gps = dontCheck super.gps;
    map-syntax = doJailbreak super.map-syntax;
    zlib-enum = doJailbreak super.zlib-enum;
    reflex-jsx = self.callPackage ./reflex-jsx.nix {};
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
      version = "2";
      sha256 = "1nd8pzsdan6zxddm96xswcm67g43zkbj1rm3m3wx3as4jj3qmw7m";
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
    focus-intercom = self.mkDerivation ({
      pname = "focus-intercom";
      version = "0.1";
      src = filterGitSource ./intercom;
      license = nixpkgs.stdenv.lib.licenses.bsd3;
      buildDepends = with self; [
        text focus-js focus-core reflex reflex-dom-core
      ];
    });
    twilio = overrideCabal super.twilio (drv: {
      jailbreak = true;
    });
    # https://github.com/vincenthz/hs-connection/pull/30
    connection = overrideCabal super.connection (drv: {
      src = nixpkgs.fetchFromGitHub {
        owner = "snoyberg";
        repo = "hs-connection";
        rev = "f5c88f81e7fcca0e851c0ef9d01d1e702bb03ea1";
        sha256 = "1jiypxkwlknrnmyspc2w87qxb7bd45g89r8mizf22nd95h7vmcrc";
      };
    });
    http2-client = dontCheck (overrideCabal super.http2-client (drv: {
      src = nixpkgs.fetchFromGitHub {
        owner = "lucasdicioccio";
        repo = "http2-client";
        rev = "60ae70e0312b46972b4e2702f698d7c038ddf5f6";
        sha256 = "0cxlnhriq3i58z8mdqd1skwy5b9ga01innwa0yi84iivadsj26b8";
      };
    }));
    push-notify-apn = addBuildDepend (overrideCabal super.push-notify-apn (drv: {
      src = nixpkgs.fetchFromGitHub {
        owner = "wrinkl";
        repo = "push-notify-apn";
        rev = "4fead216bfc4e4901e075f408fe5cc4f901d1f0f";
        sha256 = "11y9krlg6rh5syvhr6zphq6yya905ir67l05jg2bn1prgyd0l35z";
      };
    })) self.http2-client;
    phone-push = addBuildDepend (addBuildDepend (overrideCabal super.phone-push (drv: {
      src = nixpkgs.fetchFromGitHub {
        owner = "wrinkl";
        repo = "haskell-phone-push";
        rev = "b275df410e39c340788daa68f7160c2fdc428ad4";
        sha256 = "1fa10n8gp7maj474jlsly9ikia1i08s4w3da6y3yzralnbqf6lby";
      };
    })) self.casing) self.data-default;
    MailchimpSimple = overrideCabal super.MailchimpSimple (drv: {
      src = nixpkgs.fetchFromGitHub {
        owner = "obsidiansystems";
        repo = "MailchimpSimple";
        rev = "24d73548a569a22265cb7ee20c621733d9698afa";
        sha256 = "1g2lz7b84hwnjgiv3sabzhcj069x06cly018cc0s1w9r2m5r2bf1";
      };
    });
  }
