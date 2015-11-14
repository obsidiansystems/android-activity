{ nixpkgs }:

with (import ./lib.nix { inherit nixpkgs; });
let
  inherit (nixpkgs) stdenv;
  overrideCabal = drv: f: if isNull drv then null else (drv.override (args: args // {
    mkDerivation = drv: args.mkDerivation (drv // f drv);
  })) // { overrideScope = scope: overrideCabal (drv.overrideScope scope) f;
         };
in self: super: {
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
    HList = self.callPackage
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
       }) { inherit (nixpkgs) diffutils;};
    groundhog = self.mkDerivation ({
      pname = "groundhog";
      version = "0.7.0.3";
      src = ./groundhog/groundhog;
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
    dependent-sum-template = overrideCabal super.dependent-sum-template (drv: {
      version = "0.0.0.4";
      src = nixpkgs.fetchgit {
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
      src = nixpkgs.fetchgit {
        url = git://github.com/ryantrinkle/diagrams-svg;
        rev = "1abe6f74e9111b1c7efffed7fa693feacc7b6029";
        sha256 = "f02fddb399989fe35ad495602f615b7c412eea5b6a7e3cf4baeb16d2a5f548ef";
      };
    });
    lucid-svg = overrideCabal super.lucid-svg (drv: {
      version = "0.5.0.0";
      sha256 = "1p7ipdy0nmqfg1b038a1b5nd3xh2779d2gnw4h683mm5jcbf0mvj";
    });
  }
