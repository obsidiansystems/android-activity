{ enableProfiling ? false
, runWithHeapProfiling ? false
, enableExposeAllUnfoldings ? true
, enableTraceReflexEvents ? false
, iosSdkVersion ? "10.2"
, useZopfli ? true
, withHoogle ? false
, extraNginxServerConfig ? ""
}:
assert runWithHeapProfiling -> enableProfiling;
let tryReflex' = import ./reflex-platform {
      inherit enableExposeAllUnfoldings enableTraceReflexEvents;
      enableLibraryProfiling = enableProfiling;
      useReflexOptimizer = false;
      iosSdkVersion = iosSdkVersion;
    };
    inherit (tryReflex'.nixpkgs) lib;
    defaultAndroidConfig = {
      packagePrefix = "systems.obsidian";
      # URI information that becomes AndroidManifest.xml content for additional intent filters.
      # Expected format: [scheme domain port subdomain_pattern]
      # E.g., ["https:" "obsidian.systems" ":8000" "*."]
      deepLinkUris = [];
      # AndroidManifest.xml content for additional permissions.
      permissions = "";
      icon = "@drawable/ic_launcher";
      googleServicesJson = null;
      includeFirebaseService = true;
      services = ''
        <service android:name=".LocalFirebaseInstanceIDService">
        <intent-filter>
        <action android:name="com.google.firebase.INSTANCE_ID_EVENT"/>
        </intent-filter>
        </service>
        <service android:name=".LocalFirebaseMessagingService">
          <intent-filter>
            <action android:name="com.google.firebase.MESSAGING_EVENT"/>
          </intent-filter>
        </service>
      '';
      dependencies = ''
        compile 'com.android.support:appcompat-v7:25.3.0'
        compile 'com.google.firebase:firebase-messaging:10.2.0'
        compile 'com.firebase:firebase-jobdispatcher:0.5.2'
      '';
    };
    mkAndroidIntentFilter = x: # x :: ["scheme:" "host" ":port" "subdomain_pattern"], see 'androidConfig.deepLinkUris'
      let protocol = lib.strings.removeSuffix ":" (builtins.elemAt x 0);
          host = builtins.elemAt x 1;
          port = lib.strings.removePrefix ":" (builtins.elemAt x 2);
          prefix = builtins.elemAt x 3;
      in ''
        <intent-filter android:autoVerify="true">
          <action android:name="android.intent.action.VIEW" />
          <category android:name="android.intent.category.DEFAULT" />
          <category android:name="android.intent.category.BROWSABLE" />
          <data android:scheme="${protocol}"
                android:host="${prefix + host}"
                ${ if (lib.strings.stringLength port > 0) then ("android:port=\"" + port + "\"") else "" }
                android:pathPrefix="/" />
        </intent-filter>
      '';

in lib.makeExtensible (focusSelf:
let inherit (focusSelf) filterGitSource mkDerivation nixpkgs pkgs stdenv tryReflex;
in with nixpkgs.haskell.lib; {
  tryReflex = tryReflex' // (if withHoogle then {
    ghc = tryReflex'.ghc.override {
      overrides = _: su: {
        ghcWithPackages = su.ghcWithHoogle;
      };
    };
  } else {});
  inherit (focusSelf.tryReflex) nixpkgs;
  inherit (focusSelf.nixpkgs) stdenv pkgs;

  backendHaskellPackagesBase = tryReflex.ghc;
  frontendHaskellPackagesBase = tryReflex.ghcjs;
  androidPackagesBase = {
    "arm64-v8a" = {
      nixpkgsAndroid = tryReflex.nixpkgsCross.android.arm64Impure;
      androidHaskellPackagesBase = tryReflex.ghcAndroidArm64;
    };
    "armeabi-v7a" = {
      nixpkgsAndroid = tryReflex.nixpkgsCross.android.armv7aImpure;
      androidHaskellPackagesBase = tryReflex.ghcAndroidArmv7a;
    };
  };
  iosSimulatorHaskellPackagesBase = tryReflex.ghcIosSimulator64;
  iosArm64HaskellPackagesBase = tryReflex.ghcIosArm64;

  myPostgres = nixpkgs.postgresql95; #TODO: shouldn't be exposed
  filterGitSource = p: if builtins.pathExists p then builtins.filterSource (path: type: !(builtins.elem (baseNameOf path) [ ".git" "tags" "TAGS" "dist" ])) p else null;
  mkDerivation = nixpkgs.lib.makeOverridable (
    { name
    , version
    , backendDepends ? (p: [])
      # Packages in backendTools are made available both at build time and at runtime for the backend
    , backendTools ? (p: [])
    , frontendDepends ? (p: [])
    , frontendTools ? (p: [])
    , backendTestDepends ? (p: [])
    , webDriverTestDepends ? (p: [])
    , backendTestTools ? (p: [])
    , webDriverTestTools ? (p: [])
    , commonDepends ? (p: [])
    , commonTools ? (p: [])
    , haskellPackagesOverrides ? self: super: {}
    , fixupStatic ? (x: x)
    , fixupStaticForBackend ? (x: x)
    , staticSrc ? fixupStatic (filterGitSource ../static)
    , overrideServerConfig ? (args: outputs: x: x)
    , mobileExecutable ? "mobile.hs"
    , androidConfig ? {}
    , iosConfig ? {} # For arguments, see reflex-platform/ios/default.nix
    }:
    let
      # Break recursion
      appName = name;
      appVersion = version;

      frontendSrc = filterGitSource ../frontend;
      commonSrc = filterGitSource ../common;
      backendSrc = filterGitSource ../backend;
      marketingSrc = filterGitSource ../marketing;
      backendTestsSrc = filterGitSource ../tests/backend;
      webDriverTestsSrc = filterGitSource ../tests/webdriver;

      effectiveAndroidConfig = defaultAndroidConfig // androidConfig;

      frontendHaskellPackages = extendFrontendHaskellPackages focusSelf.frontendHaskellPackagesBase;
      frontendGhcHaskellPackages = extendFrontendHaskellPackages tryReflex.ghc;
      backendHaskellPackages = extendBackendHaskellPackages focusSelf.backendHaskellPackagesBase;
      androidPackages = lib.mapAttrs (abiVersion: { nixpkgsAndroid, androidHaskellPackagesBase }: {
        inherit nixpkgsAndroid;
        androidHaskellPackages = androidHaskellPackagesBase.override {
          overrides = self: super: sharedOverrides self super // {
            mkDerivation = drv: super.mkDerivation (drv // {
              dontStrip = true;
              enableSharedExecutables = false;
              configureFlags = (drv.configureFlags or []) ++ [
              ];
            });
          };
        };
      }) focusSelf.androidPackagesBase;
      iosSimulatorHaskellPackages = focusSelf.iosSimulatorHaskellPackagesBase.override {
        overrides = self: super: let new = sharedOverrides self super; in new // {
          focus-js = addBuildDepend new.focus-js self.jsaddle-wkwebview;
        };
      };
      iosAArch64HaskellPackages = focusSelf.iosArm64HaskellPackagesBase.override {
        overrides = self: super: let new = sharedOverrides self super; in new // {
          focus-js = addBuildDepend new.focus-js self.jsaddle-wkwebview;
          jsaddle = overrideCabal super.jsaddle (drv: {
            configureFlags = (drv.configureFlags or []) ++ [
              # "-f-include-app-delegate"
            ];
          });
        };
      };

      haddockWhenWithHoogle = drv: if withHoogle then drv else dontHaddock drv;
      exposeAllUnfoldings = drv: appendConfigureFlag drv "--ghc-options=-fexpose-all-unfoldings";
      focusFlags = drv: haddockWhenWithHoogle (exposeAllUnfoldings drv);
      sharedOverrides = self: super: (import ./override-shared.nix { inherit nixpkgs filterGitSource; }) self super
        // { focus-aeson-orphans = focusFlags (self.callCabal2nix "focus-aeson-orphans" (filterGitSource ./aeson-orphans) {});
             focus-core = focusFlags (self.callCabal2nix "focus-core" (filterGitSource ./core) {});
             focus-datastructures = focusFlags (self.callCabal2nix "focus-datastructures" (filterGitSource ./datastructures) {});
             focus-emojione = focusFlags (self.callCabal2nix "focus-emojione" (filterGitSource ./emojione) {});
             focus-emojione-data = focusFlags (self.callCabal2nix "focus-emojione-data" (filterGitSource ./emojione/data) {});
             focus-gitlab = focusFlags (self.callCabal2nix "focus-gitlab" (filterGitSource ./gitlab) {});
             focus-http-th = focusFlags (self.callCabal2nix "focus-http-th" (filterGitSource ./http/th) {});
             focus-js = focusFlags (overrideCabal (self.callCabal2nix "focus-js" (filterGitSource ./js) {}) (drv: {
               libraryHaskellDepends = (drv.libraryHaskellDepends or []) ++ (if self.ghc.isGhcjs or false then (with self; [ghcjs-base]) else []);
             }));
             focus-pivotal = haddockWhenWithHoogle (self.callCabal2nix "focus-pivotal" (filterGitSource ./pivotal) {});
             focus-serve = haddockWhenWithHoogle (self.callCabal2nix "focus-serve" (filterGitSource ./http/serve) {});
             focus-th = haddockWhenWithHoogle (self.callCabal2nix "focus-th" (filterGitSource ./th) {});
             focus-webdriver = haddockWhenWithHoogle (self.callCabal2nix "focus-webdriver" (filterGitSource ./webdriver) {});
             email-parse = haddockWhenWithHoogle (self.callCabal2nix "email-parse" (filterGitSource ./email-parse) {});
             unique-id = haddockWhenWithHoogle (self.callCabal2nix "unique-id" (filterGitSource ./unique-id) {});
             hellosign = haddockWhenWithHoogle (self.callCabal2nix "hellosign" (filterGitSource ./hellosign) {});
             touch = haddockWhenWithHoogle (self.callCabal2nix "touch" (filterGitSource ./touch) {});
             focus-phonepush-worker = haddockWhenWithHoogle (self.callCabal2nix "focus-phonepush-worker" (filterGitSource ./phonepush-worker) {});
             focus-weblayouts = haddockWhenWithHoogle (self.callCabal2nix "focus-weblayout" (filterGitSource ./weblayouts) {});
             focus-weblayouts-backend = haddockWhenWithHoogle (self.callCabal2nix "focus-weblayouts-backend" (filterGitSource ./weblayouts-backend) {});
           };

      extendFrontendHaskellPackages = haskellPackages: (haskellPackages.override {
        overrides = self: super: sharedOverrides self super // {
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
            license = nixpkgs.stdenv.lib.licenses.bsd3;
            platforms = self.ghc.meta.platforms;
          });
        };
      }).override { overrides = haskellPackagesOverrides; };
      extendBackendHaskellPackages = haskellPackages: (haskellPackages.override {
        overrides = self: super:
          let gargoylePkgs = import ./gargoyle self;
          in sharedOverrides self super // rec {
            focus-backend = focusFlags (self.callPackage ./backend { inherit (focusSelf) myPostgres; });
            focus-client = focusFlags (self.callPackage ./client {});
            focus-heremaps = focusFlags (self.callPackage ./heremaps {});
            focus-test = focusFlags (self.callPackage ./test {});
            websockets = focusFlags (overrideCabal super.websockets (drv: {
              src = filterGitSource ./websockets;
              buildDepends = with self; [ pipes pipes-bytestring pipes-parse pipes-attoparsec pipes-network ];
              jailbreak = true;
            }));
            websockets-snap = focusFlags (overrideCabal super.websockets-snap (drv: {
              src = filterGitSource ./websockets-snap;
              buildDepends = with self; [ snap-core snap-server io-streams ];
            }));
            snap-stream = focusFlags (self.callCabal2nix "snap-stream" (filterGitSource ./snap-stream) {});
            gargoyle = gargoylePkgs.gargoyle;
            gargoyle-postgresql = gargoylePkgs.gargoyle-postgresql;
            gargoyle-nix = self.callPackage ./gargoyle-nix { postgresql = focusSelf.myPostgres; };
          };
      }).override { overrides = haskellPackagesOverrides; };


      mkAssets = let assetsMaker = (import ./http/assets.nix { inherit nixpkgs; });
                 in if useZopfli then assetsMaker.mkAssets else assetsMaker.mkAssetsWith assetsMaker.gzipEncodings;


      libraryHeader = ''
        library
          exposed-modules: $(find -L * -name '[A-Z]*.hs' | sed 's/\.hs$//' | grep -vi '^main'$ | tr / . | tr "\n" , | sed 's/,$//')
      '';
      mkCabalFile = haskellPackages: pname: executableName: depends: src: rtsOpts:
        let inherit (pkgs.lib) optionalString;
            mkCabalTarget = header: ''
              ${header}
                hs-source-dirs: .
                build-depends: ${pkgs.lib.concatStringsSep "," ([ "base" "bytestring" "containers" "time" "transformers" "text" "lens" "aeson" "mtl" "directory" "deepseq" "binary" "async" "vector" "template-haskell" "filepath" "primitive" "ghc-prim" ] ++ (if haskellPackages.ghc.isGhcjs or false then [ "ghcjs-base" "ghcjs-prim" ] else [ "process" "unix"]) ++ builtins.filter (x: x != null) (builtins.map (x: x.pname or null) depends))}
                other-extensions: TemplateHaskell
                ghc-options: -threaded -Wall -fwarn-tabs -fno-warn-unused-do-bind -funbox-strict-fields -fprof-auto -rtsopts -threaded "-with-rtsopts=${rtsOpts}" -fspecialise-aggressively
                default-language: Haskell2010
                default-extensions: NoDatatypeContexts
                if impl(ghcjs)
                  cpp-options: -DGHCJS_GC_INTERVAL=60000 -DGHCJS_BUSY_YIELD=6 -DGHCJS_SCHED_QUANTUM=5
                  ghcjs-options: -dedupe
                if !os(ios) && !arch(aarch64) && !arch(arm)
                  cpp-options: -DUSE_TEMPLATE_HASKELL
                if os(ios) || arch(aarch64) || arch(arm)
                  cpp-options: -DMOBILE
                if os(ios)
                  if arch(aarch64)
                    ld-options: -isysroot /Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS${iosSdkVersion}.sdk
                  else
                    ld-options: -isysroot /Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator10.0.sdk
            '';
            defaultMain = ''$(ls "${src}" | grep -i '^\(${executableName}\|main\)\.\(l\|\)hs'$)'';
            executableHeader = executableName: mainFile:
              if mainFile != null then
                ''executable ${executableName}
                    main-is: ${mainFile}
                ''
              else "";
        in nixpkgs.runCommand "${pname}.cabal" {} ''
        cat > "$out" <<EOF
        name: ${pname}
        version: ${appVersion}
        cabal-version: >= 1.24
        build-type: Simple

        ${"" /*mkCabalTarget libraryHeader*/ /* Disabled because nothing was actually building libraries anyhow */}

        ${if executableName != null then mkCabalTarget (executableHeader executableName defaultMain) else ""}

        $(for x in $(ls "${src}" | sed -n 's/\([a-z].*\)\.hs$/\1/p' | grep -vi '^main'$) ; do
            cat <<INNER_EOF
        ${mkCabalTarget (executableHeader "$x" "$x.hs")}
        INNER_EOF
        done
        )


        EOF
      '';

      #TODO: The list of builtin packages should be in nixpkgs, associated with the compiler
      mkPreConfigure = pname: cabalFile: /*haskellPackages: pname: executableName: depends: src:*/ ''
        if ! ls | grep ".*\\.cabal$" ; then
          ln -s "${cabalFile}" "${pname}.cabal"
        fi
        cat *.cabal
      '';

      mkFrontend = frontendSrc: commonSrc: haskellPackages: static: additionalDeps:
        haskellPackages.callPackage ({mkDerivation, focus-core, focus-js, ghcjs-dom}:
          # TODO: Make frontend-src symbolic linking more resilient to nullglobs
          mkDerivation (rec {
            pname = "${appName}-frontend";
            version = appVersion;
            license = null;
            src = nixpkgs.runCommand "frontend-src" {
              buildCommand = ''
                mkdir "$out"
                ${if commonSrc != null then ''ln -s "${commonSrc}"/src/* "$out"/'' else ""}
                ${if frontendSrc != null
                  then ''ln -s "${frontendSrc}"/src{,-bin}/* "$out"/''
                  else ""}

              '';
            } "";
            preConfigure = mkPreConfigure pname passthru.cabalFile;
            preBuild = ''
              ${if static == null then "" else ''ln -sfT ${static} static''}
            '';
            buildDepends = [
              focus-core
              focus-js
              ghcjs-dom
            ] ++ frontendDepends haskellPackages ++ commonDepends haskellPackages ++ additionalDeps;
            buildTools = [] ++ frontendTools pkgs;
            isExecutable = true;
            passthru = {
              inherit haskellPackages;
              cabalFile = mkCabalFile haskellPackages pname "frontend" buildDepends src "-T";
            };
            doHaddock = withHoogle;
          })) {};

      ghcjsApp = pkgs.stdenv.mkDerivation (rec {
        name = "ghcjs-app";
        unminified = mkFrontend frontendSrc commonSrc frontendHaskellPackages staticSrc [];
        ghcjsExterns = ./ghcjs.externs.js;
        inherit (pkgs) closurecompiler;
        builder = builtins.toFile "builder.sh" ''
          source "$stdenv/setup"

          mkdir -p "$out"
          cd "$out"
          for x in $(ls "$unminified/bin" | grep '\.jsexe'$) ; do
            mkdir "$x"
            pushd "$x"
            ln -s "$unminified/bin/$x/all.js" all.unminified.js
            java -Xmx16384m -XX:MaxNewSize=5120m -XX:NewSize=5120m -jar "$closurecompiler/share/java/"closure-compiler-v*.jar --externs "$ghcjsExterns" -O ADVANCED --create_source_map="all.js.map" --source_map_format=V3 --js_output_file="all.js" all.unminified.js
            echo "//# sourceMappingURL=all.js.map" >> all.js
            popd
          done

        '';
        buildInputs = with pkgs; [
          jre
        ];
        passthru = {
          frontend = unminified;
        };
      });

      mkCLibCabalFile = haskellPackages: pname: depends: src: nixpkgs.runCommand "${pname}.cabal" {} ''
          cat > "$out" <<EOF
          name: ${pname}
          version: ${appVersion}
          build-type: Simple
          cabal-version: >= 1.8

          executable lib${appName}.so
            build-depends: ${pkgs.lib.concatStringsSep "," ([ "base" "bytestring" "containers" "time" "transformers" "text" "lens" "aeson" "mtl" "directory" "deepseq" "binary" "async" "vector" "template-haskell" "filepath" "primitive" "ghc-prim" "jsaddle-clib" ] ++ (if haskellPackages.ghc.isGhcjs or false then [ "ghcjs-base" "ghcjs-prim" ] else [ "process" "unix"]) ++ builtins.filter (x: x != null) (builtins.map (x: x.pname or null) depends))}
            default-language: Haskell2010
            cc-options: -shared -fPIC
            ld-options: -shared
            other-modules: $(cd "${src}" ; find -L * -name '[A-Z]*.hs' | sed 's/\.hs$//' | grep -vi '\(\.splices\|main\)'$ | tr / . | tr "\n" , | sed 's/,$//')
            include-dirs: cbits/include
            includes: jni.h
            hs-source-dirs: .
            c-sources: cbits/focus.c
            main-is: ${mobileExecutable}
            ghc-options: -shared -fPIC -threaded -no-hs-main -Wall -fwarn-tabs -fno-warn-unused-do-bind -funbox-strict-fields -O2 -fprof-auto -lHSrts_thr -lCffi -lm -llog
            cpp-options: -DMOBILE
            default-extensions: NoDatatypeContexts
          EOF
        '';

      mkCLibFrontend =
        let crossHs = ./cross-android/hs;
            packageName = effectiveAndroidConfig.packagePrefix + "." + appName;
            packageJNIName = builtins.replaceStrings ["."] ["_"] packageName;
        in frontendSrc: commonSrc: haskellPackages: static: additionalDeps:
             haskellPackages.callPackage ({mkDerivation, focus-core, focus-js, ghcjs-dom}:
                mkDerivation (rec {
                  pname = "${appName}-frontend-clib";
                  version = appVersion;
                  license = null;
                  # TODO: Make frontend-src symbolic linking more resilient to nullglobs
                  src = nixpkgs.runCommand "frontend-src" {
                    buildCommand = ''
                      mkdir "$out"
                      ${if commonSrc != null then ''ln -s "${commonSrc}"/src/* "$out"/'' else ""}
                      ${if frontendSrc != null
                          then ''ln -s "${frontendSrc}"/src{,-bin}/* "$out"''
                          else ""
                       }
                      cp -r --no-preserve=mode "${crossHs}/cbits" "$out"
                      sed -i 's|systems_obsidian_focus|'"${packageJNIName}"'|' "$out/cbits/"*"."{c,h}
                    '';
                  } "";
                  preConfigure = ''
                    ln -s "${passthru.cabalFile}" "${pname}.cabal"
                  '';
                  preBuild = ''
                    ${if static == null then "" else ''ln -sfT ${static} static''}
                  '';
                  buildDepends = [
                    focus-core
                    focus-js
                  ] ++ frontendDepends haskellPackages ++ commonDepends haskellPackages ++ additionalDeps;
                  buildTools = [] ++ frontendTools pkgs;
                  isExecutable = false;
                  isLibrary = false;
                  passthru = {
                    inherit haskellPackages;
                    cabalFile = mkCLibCabalFile haskellPackages pname buildDepends src;
                  };
                  doHaddock = false;
                })) {};

      result =  pkgs.stdenv.mkDerivation (rec {
        name = "${appName}-${appVersion}";
        staticAssets = mkAssets (fixupStaticForBackend staticSrc);
        zoneinfo = ./zoneinfo;
        frontendJsAssets = mkAssets "${ghcjsApp}";
        ${if builtins.pathExists ../marketing then "marketing" else null} = marketingSrc;
        # Give the minification step its own derivation so that backend rebuilds don't redo the minification
        frontend = ghcjsApp;
        frontend_ = frontend;
        emails = if builtins.pathExists ../emails then (import ./emails {}).build else null;
        builder = builtins.toFile "builder.sh" ''
          source "$stdenv/setup"
          set -x

          mkdir -p "$out"
          ln -s "$staticAssets" "$out/static.assets"
          if ! [ -z "''${marketing+x}" ] ; then
            ln -s "$marketing" "$out/marketing"
          fi
          ln -s "$backend/bin/"* "$out"
          ln -s "$frontendJsAssets" "$out/frontendJs.assets"
          ln -s "$zoneinfo" "$out/zoneinfo"
          if [ -n "''${androidApp+x}" ] ; then
            ln -s "$androidApp" "$out/android"
          fi
          if [ -n "''${emails+x}" ] ; then
            ln -s "$emails" "$out/emails"
          fi
        '';
        backend =
          backendHaskellPackages.callPackage ({mkDerivation, vector-algorithms, focus-serve, focus-core, focus-backend}: mkDerivation (rec {
            pname = "${appName}-backend";
            license = null;
            version = appVersion;
            src = nixpkgs.runCommand "backend-src" {
              buildCommand = ''
                mkdir "$out"
                ${if commonSrc != null then ''ln -s "${commonSrc}"/src/* "$out"/'' else ""}
                ln -s "${backendSrc}"/src{,-bin}/* "$out"/

                shopt -s nullglob
                frontendFiles=("${frontendSrc}"/src/*)
                for f in ''${frontendFiles[@]} ; do
                    if echo "$f" | grep -vq "/Main.\(hs\|lhs\)$" ; then
                        ln -s "$f" "$out"/
                    fi
                done
              '';
            } "";

            preConfigure = mkPreConfigure pname passthru.cabalFile;
            preBuild = ''
              ${if staticSrc == null then "" else ''ln -sfT ${staticSrc} static''}
            '';
            buildDepends = [
              vector-algorithms
              focus-core focus-backend focus-serve
            ] ++ backendDepends backendHaskellPackages ++ commonDepends backendHaskellPackages ++ frontendDepends backendHaskellPackages;
            buildTools = [] ++ backendTools pkgs;
            isExecutable = true;
            configureFlags = [ "--ghc-option=-lgcc_s" "--ghc-option=-DBACKEND" ] ++ (if enableProfiling then [ "--enable-executable-profiling" ] else [ ]);
            passthru = {
              haskellPackages = backendHaskellPackages;
              cabalFile = mkCabalFile backendHaskellPackages pname "backend" buildDepends src "-N10 -I0";
            };
            doHaddock = withHoogle;
          })) {};
        passthru = rec {
          inherit tryReflex;
          ${if builtins.pathExists ../tests/webdriver then "webdriver-tests" else null} =
            backendHaskellPackages.callPackage ({mkDerivation, webdriver, focus-webdriver}: mkDerivation (rec {
              pname = "${appName}-webdriver-tests";
              license = null;
              version = appVersion;
              src = nixpkgs.runCommand "webdriver-tests-src" {
                buildCommand = ''
                  mkdir "$out"
                  ln -sf "${webDriverTestsSrc}"/src/* "$out"/
                '';
              } "";
              preConfigure = mkPreConfigure pname passthru.cabalFile;
              preBuild = ''
                ln -sfT ${staticSrc} static
              '';
              buildDepends = [ webdriver focus-webdriver ] ++ webDriverTestDepends backendHaskellPackages;
              buildTools = [] ++ webDriverTestTools pkgs;
              isExecutable = true;
              configureFlags = [ ];
              passthru = {
                haskellPackages = backendHaskellPackages;
                cabalFile = mkCabalFile backendHaskellPackages pname "webdriver-tests" buildDepends src "-N10 -I0";
              };
              doHaddock = false;
          })) {};
          ${if builtins.pathExists ../tests/webdriver then "dev-webdriver-tests" else null} =
            { seleniumHost ? "localhost", seleniumPort ? "4444"}:
            let selenium-server = nixpkgs.selenium-server-standalone;
                chromium = nixpkgs.chromium;
                google-chrome = nixpkgs.google-chrome;
                inherit webdriver-tests;
            in nixpkgs.writeScript "dev-webdriver-tests" ''
                 "${selenium-server}/bin/selenium-server" > /dev/null 2>&1 &
                 SELENIUM_PID=$!
                 sleep 5
                 "${passthru.webdriver-tests}/bin/webdriver-tests" "${seleniumHost}" "${seleniumPort}" "${chromium}/bin/chromium"
                 kill $SELENIUM_PID
               '';

          ${if builtins.pathExists ../tests/webdriver then "run-webdriver-tests" else null} =
            { seleniumHost ? "localhost", seleniumPort ? "4444"}:
            let selenium-server = nixpkgs.selenium-server-standalone;
                chromium = nixpkgs.chromium;
                google-chrome = nixpkgs.google-chrome;
                inherit webdriver-tests;
            in nixpkgs.writeScript "run-webdriver-tests" ''
                 "${backend}/bin/backend" -p 8000 > /dev/null 2>&1 &
                 BACKEND_PID=$!
                 "${selenium-server}/bin/selenium-server" > /dev/null 2>&1 &
                 SELENIUM_PID=$!
                 sleep 5
                 "${passthru.webdriver-tests}/bin/webdriver-tests" "${seleniumHost}" "${seleniumPort}" "${chromium}/bin/chromium"
                 # "${passthru.webdriver-tests}/bin/webdriver-tests" "${seleniumHost}" "${seleniumPort}" "${google-chrome}/bin/google-chrome-stable"
                 kill $BACKEND_PID
                 kill $SELENIUM_PID
               '';
          ${if builtins.pathExists ../tests/backend then "backend-tests" else null} =
            backendHaskellPackages.callPackage ({mkDerivation, vector-algorithms, focus-core, focus-client, focus-backend}: mkDerivation (rec {
                pname = "${appName}-backend-tests";
                license = null;
                version = appVersion;
                src = nixpkgs.runCommand "backend-tests-src" {
                  buildCommand = ''
                    mkdir "$out"
                    ${if commonSrc != null then ''ln -s "${commonSrc}"/src/* "$out"/'' else ""}
                    ln -s "${backendSrc}"/src/* "$out"/
                    ln -sf "${backendTestsSrc}"/src/* "$out"/
                  '';
                } "";

                preConfigure = mkPreConfigure pname passthru.cabalFile;
                preBuild = ''
                  ln -sfT ${staticSrc} static
                '';
                buildDepends = [
                  vector-algorithms
                  focus-core focus-backend focus-client
                ] ++ backendTestDepends backendHaskellPackages ++ commonDepends backendHaskellPackages ++ backendDepends backendHaskellPackages;
                buildTools = [] ++ backendTestTools pkgs;
                isExecutable = true;
                configureFlags = [ "--ghc-option=-lgcc_s" "--ghc-option=-DBACKEND" ];
                passthru = {
                  haskellPackages = backendHaskellPackages;
                  cabalFile = mkCabalFile backendHaskellPackages pname "backend-tests" buildDepends src "-N10 -I0";
                };
                doHaddock = withHoogle;
          })) {};
          frontend = frontend_.unminified;
          frontendMinified = frontend_;
          inherit staticAssets;
          frontendGhc = mkFrontend frontendSrc commonSrc frontendGhcHaskellPackages staticSrc
              (with frontendGhcHaskellPackages; [ websockets wai warp wai-app-static jsaddle jsaddle-warp th-lift-instances ]);
          frontendGhcWKWebView = mkFrontend frontendSrc commonSrc frontendGhcHaskellPackages staticSrc
              (with frontendGhcHaskellPackages; [ websockets wai warp wai-app-static jsaddle jsaddle-wkwebview ]);
          mkIosApp = args:
            tryReflex.ios.buildApp (iosConfig // args);
          mkMacApp = bundleName: bundleIdentifier: exeName: exePath: staticSrc: nixpkgs.runCommand "${exeName}-app" (rec {
            inherit exePath;
            infoPlist = builtins.toFile "Info.plist" ''
              <?xml version="1.0" encoding="UTF-8"?>
              <!DOCTYPE plist SYSTEM "file://localhost/System/Library/DTDs/PropertyList.dtd">
              <plist version="0.9">
                <dict>
                <key>CFBundleInfoDictionaryVersion</key>
                <string>6.0</string>
                <key>CFBundleIdentifier</key>
                <string>${bundleIdentifier}</string>
                <key>CFBundleDevelopmentRegion</key>
                <string>en</string>
                <key>CFBundleExecutable</key>
                <string>${exeName}</string>
                <key>CFBundleIconFile</key>
                <string></string>
                <key>CFBundleName</key>
                <string>${bundleName}</string>
                <key>CFBundlePackageType</key>
                <string>APPL</string>
                <key>CFBundleVersion</key>
                <string>1.0</string>
                <key>CFBundleShortVersionString</key>
                <string>1.0</string>
                <key>NSHumanReadableCopyright</key>
                <string>${exeName}</string>
                <key>NSPrincipalClass</key>
                <string>NSApplication</string>
                </dict>
                <key>CFBundleIcons</key>
                <dict>
                  <key>CFBundlePrimaryIcon</key>
                  <dict>
                    <key>CFBundleIconFiles</key>
                    <array>
                      <string>assets/Wrinkl iOS 60</string>
                    </array>
                  </dict>
                </dict>
              </plist>
            '';
            indexHtml = builtins.toFile "index.html" ''
              <html>
                <head>
                </head>
                <body>
                </body>
              </html>
            '';
            run = builtins.toFile "run" ''
              #!/usr/bin/env bash
              set -euo pipefail

              function cleanup {
                if [ -n "$tmpdir" -a -d "$tmpdir" ]; then
                  echo "Cleaning up tmpdir" >&2
                  chmod -R +w $tmpdir
                  rm -fR $tmpdir
                fi
              }

              trap cleanup EXIT

              tmpdir=$(mktemp -d)

              mkdir -p $tmpdir
              cp -LR "$(dirname $0)/../${exeName}.app" $tmpdir
              chmod +w "$tmpdir/${exeName}.app"
              mkdir -p "$tmpdir/${exeName}.app/config"
              cp "$1" "$tmpdir/${exeName}.app/config/route"
              open "$tmpdir/${exeName}.app"
            '';
          }) ''
            set -x
            mkdir -p "$out/${exeName}.app"
            ln -s "$infoPlist" "$out/${exeName}.app/Info.plist"
            ln -s "$indexHtml" "$out/${exeName}.app/index.html"
            mkdir -p "$out/bin"
            cp --no-preserve=mode "$run" "$out/bin/run-in-sim"
            chmod +x "$out/bin/run-in-sim"
            ln -s "$exePath/${exeName}" "$out/${exeName}.app/"
            cp -RL "${staticSrc}"/* "$out/${exeName}.app/"
          '';
          frontendIosSimulator = mkFrontend frontendSrc commonSrc iosSimulatorHaskellPackages staticSrc
              (with iosSimulatorHaskellPackages; [ jsaddle jsaddle-wkwebview ]);
          frontendIosSimulatorApp = mkIosApp { bundleName = "mobile";
                                               bundleIdentifier = "mobile";
                                               bundleVersionString = "1.0";
                                               bundleVersion = "1";
                                               executableName = "mobile";
                                               package = p: frontendIosSimulator;
                                               staticSrc = staticSrc;
                                               apsEnv = "development";
                                             };
          frontendIosAArch64 = mkFrontend frontendSrc commonSrc iosAArch64HaskellPackages staticSrc
              (with iosAArch64HaskellPackages; [ jsaddle jsaddle-wkwebview ]);
          frontendIosAArch64App = mkIosApp { bundleName = "mobile";
                                             bundleIdentifier = "mobile";
                                             bundleVersionString = "1.0";
                                             bundleVersion = "1";
                                             executableName = "mobile";
                                             package = p: frontendIosAArch64;
                                             staticSrc = staticSrc;
                                             apsEnv = "development";
                                           };
          androidSOs = lib.mapAttrs (abiVersion: { nixpkgsAndroid, androidHaskellPackages }: rec {
            inherit (nixpkgsAndroid.buildPackages) patchelf;
            inherit (nixpkgsAndroid) libiconv;
            inherit androidHaskellPackages;
            hsApp = mkCLibFrontend frontendSrc commonSrc androidHaskellPackages staticSrc (with androidHaskellPackages; [ jsaddle jsaddle-clib ]);
          }) androidPackages;
          androidSrc = verCode: verName:
            import ./cross-android/android {
              inherit (tryReflex) nixpkgs;
              name = appName;
              appSOs = androidSOs;
              packagePrefix = effectiveAndroidConfig.packagePrefix;
              googleServicesJson = effectiveAndroidConfig.googleServicesJson;
              additionalDependencies = effectiveAndroidConfig.dependencies;
              iconResource = effectiveAndroidConfig.icon;
              services = effectiveAndroidConfig.services;
              includeFirebase = effectiveAndroidConfig.includeFirebaseService;
              assets = nixpkgs.runCommand "android_asset" {} ''
                mkdir "$out"
                mkdir "$out"/zoneinfo
                [ "$(ls -A ${staticSrc})" ] && cp -r --no-preserve=mode "${staticSrc}"/* "$out" || echo "static folder empty. skipping..."
                cp -r --no-preserve=mode "${zoneinfo}"/* "$out/zoneinfo"
              '';
              res = nixpkgs.runCommand "android_res" {} ''
                mkdir "$out"
                if [ -e "${staticSrc}"/assets/res ]
                  then cp -r --no-preserve=mode "${staticSrc}"/assets/res/* "$out"
                fi
              '';
              versionName = verName;
              versionCode = verCode;
              intentFilters = lib.strings.concatStrings (map mkAndroidIntentFilter effectiveAndroidConfig.deepLinkUris);
              permissions = effectiveAndroidConfig.permissions;
            };
          androidApp = { key ? { store = ./keystore; alias = "focus"; password = "password"; aliasPassword = "password"; },  version ? { code = "1"; name = "1.0"; } }: tryReflex.nixpkgs.androidenv.buildGradleApp {
            acceptAndroidSdkLicenses = true;
            buildDirectory = "./.";
            gradleTask = "assemble";
            keyAlias = key.alias;
            keyAliasPassword = key.aliasPassword;
            keyStore = key.store;
            keyStorePassword = key.password;
            mavenDeps = import ./android_deps;
            name = appName;
            platformVersions = [ "25" ];
            release = true;
            src = androidSrc version.code version.name;
            useExtraSupportLibs = true;
            useGoogleAPIs = true;
            useNDK = true;
          };
          androidEmulate = tryReflex.nixpkgs.androidenv.emulateApp {
            name = appName;
            app = androidApp;
            platformVersion = "23";
            enableGPU = true;
            abiVersion = "arm64-v8a";
            useGoogleAPIs = false;
            package = androidPackagePrefix + "." + appName;
            activity = ".MainActivity";
          };
          nixpkgs = pkgs;
          backendService = {user, port}: {
            wantedBy = [ "multi-user.target" ];
            after = [ "network.target" ];
            restartIfChanged = true;
            path = backendTools pkgs;
            script = ''
              ln -sft . "${result}"/*
              mkdir -p log
              exec ./backend -p "${builtins.toString port}" ${if runWithHeapProfiling then "+RTS -hc -L200 -RTS" else ""} >>backend.out 2>>backend.err </dev/null
            '';
            serviceConfig = {
              User = user;
              KillMode = "process";
              WorkingDirectory = "~";
              Restart = "always";
              RestartSec = 5;
            };
          };
          defaultBackendPort = 8000;
          defaultBackendUid = 500;
          defaultBackendGid = 500;
          qemuInfoBlockDelimiter = "GUEST INFO";
          countCharsWhere = f: str: builtins.length (builtins.filter f (pkgs.lib.stringToCharacters str));
          completeServer = { hostName
                           , ssl ? false
                           , adminEmail ? "ops@obsidian.systems"
                           , extraRootKey ? null
                           , sslSubdomains ?
                             if (countCharsWhere (c: c == ".") hostName == 1)
                             then ["www"] # We assume that domains of the form "abc.xyz" will also have a www subdomain, but domains with more components will not
                             else []
                           , sslBehindLoadBalancer ? false
                           }:
            assert !(ssl && sslBehindLoadBalancer);
            let acmeWebRoot = "/srv/acme/";
                nginxService = {locations}:
                  let locationConfig = path: port: ''
                        location ${path} {
                          ${if path != "/" then "rewrite ^${path}(.*)$ /$1 break;" else ""}
                          proxy_pass http://127.0.0.1:${builtins.toString port};
                          proxy_set_header Host $http_host;
                          proxy_read_timeout 300s;
                          proxy_max_temp_file_size 4096m;
                          client_max_body_size 1G;
                        }
                      '';
                      locationConfigs = pkgs.lib.concatStringsSep "\n" (builtins.attrValues (pkgs.lib.mapAttrs locationConfig locations));
                  in {
                    enable = true;
                    httpConfig = if ssl then ''
                      server {
                        listen 443 ssl;
                        ssl_certificate /var/lib/acme/${hostName}/fullchain.pem;
                        ssl_certificate_key /var/lib/acme/${hostName}/key.pem;
                        ssl_protocols  TLSv1 TLSv1.1 TLSv1.2;  # don't use SSLv3 ref: POODLE
                        ${locationConfigs}
                        access_log off;
                        ${extraNginxServerConfig}
                      }
                      error_log  /var/log/nginx_error.log  warn;
                    '' else ''
                      server {
                        listen 80;
                        ${locationConfigs}
                        ${lib.optionalString sslBehindLoadBalancer ''
                          if ($http_x_forwarded_proto = 'http') {
                            return 301 https://$host$request_uri;
                          }
                        ''}
                        access_log off;
                        ${extraNginxServerConfig}
                      }
                      error_log  /var/log/nginx_error.log  warn;
                    '';
                  };
                system = "x86_64-linux";
                configuration = args@{ config, pkgs, ... }: overrideServerConfig args { inherit defaultBackendPort defaultBackendUid defaultBackendGid frontend backend backendService nginxService; } {
                  environment.systemPackages = with pkgs; [
                    rsync
                    emacs25-nox
                    git
                    rxvt_unicode.terminfo
                    focusSelf.myPostgres
                  ];

                  networking = {
                    inherit hostName;
                    firewall.allowedTCPPorts = [
                      25
                      80
                    ] ++ (if ssl then [ 443 ] else []);
                  };

                  services = {
                    journald.rateLimitBurst = 0;

                    nginx = nginxService {
                      locations = {
                        "/" = defaultBackendPort;
                      };
                    };
                  } // (if !ssl then {} else {
                    lighttpd = {
                      enable = true;
                      document-root = acmeWebRoot;
                      port = 80;
                      enableModules = [ "mod_redirect" ];
                      extraConfig = ''
                        $HTTP["url"] !~ "^/\.well-known/acme-challenge" {
                          $HTTP["host"] =~ "^.*$" {
                            url.redirect = ( "^.*$" => "https://%0$0" )
                          }
                        }
                      '';
                    };
                  });

                  systemd.services.backend = backendService {
                    user = "backend";
                    port = defaultBackendPort;
                  };
                  systemd.services.smtpProxy = {
                    wantedBy = [ "multi-user.target" ];
                    after = [ "network.target" ];
                    restartIfChanged = true;
                    script = ''
                      exec ${nixpkgs.redir}/bin/redir :25 :2525
                    '';
                    serviceConfig = {
                      User = "root";
                      KillMode = "process";
                    };
                  };
                  users.extraUsers.backend = {
                    description = "backend server user";
                    home = "/var/lib/backend";
                    createHome = true;
                    isSystemUser = true;
                    uid = defaultBackendUid;
                    group = "backend";
                  };
                  users.extraGroups.backend.gid = defaultBackendUid;
                } // (if !ssl then {} else {
                  security.acme.certs.${hostName} = {
                    webroot = acmeWebRoot;
                    email = adminEmail;
                    plugins = [ "fullchain.pem" "key.pem" "account_key.json" ];
                    postRun = ''
                      systemctl reload-or-restart nginx.service
                    '';
                    extraDomains = builtins.listToAttrs (map (subdomain: { name = "${subdomain}.${hostName}"; value = null; }) sslSubdomains);
                  };
                });
                nixos = import "${nixpkgs.path}/nixos";
            in {
              aws = nixos {
                inherit system;
                configuration = args@{ config, pkgs, ... }: {
                  imports = [
                    configuration
                    "${nixpkgs.path}/nixos/modules/virtualisation/amazon-image.nix"
                  ];

                  ec2.hvm = true;
                };
              };
              qemu = nixos {
                inherit system;
                configuration = args@{ config, pkgs, ... }: {
                  imports = [
                    configuration
                  ];

                  services.openssh.enable = true;
                  users.users.root.openssh.authorizedKeys.keys =
                    if extraRootKey == null
                    then []
                    else [ extraRootKey ];

                  system.activationScripts.shutdownOnRootLogout = {
                    text =
                      let bashProfile = builtins.toFile "bash_profile" ''
                            echo -n "Waiting for network... (ctrl-c to abort)"

                            MY_IP=""
                            for i in {1..50} ; do
                              MY_IP="$(ip -4 addr show eth0 | grep -oP '(?<=inet\s)\d+(\.\d+){3}')"
                              if [ -n "$MY_IP" ] ; then
                                break
                              fi
                              sleep 0.1
                            done

                            echo
                            echo "BEGIN ${qemuInfoBlockDelimiter}"
                            echo "MY_IP=$MY_IP"
                            echo "MY_HOST_KEY=$(printf "%q" "$(cat /etc/ssh/ssh_host_ed25519_key.pub)")"
                            echo "END ${qemuInfoBlockDelimiter}"
                          '';
                      in ''
                        echo "systemctl poweroff" >/root/.bash_logout
                        ln -sfT ${bashProfile} /root/.bash_profile
                      '';
                    deps = [];
                  };

                  services.mingetty.autologinUser = "root";

                  virtualisation = {
                    graphics = false;
                    qemu.networkingOptions = [
                      "-net nic,model=virtio"
                      "-net vde,sock=/var/run/vde.ctl"

                      # We can't keep the following, or the VDE-based networking fails
                      # This may violate user expectations, since QEMU_NET_OPTS is not available
                      #"-net user\${QEMU_NET_OPTS:+,$QEMU_NET_OPTS}"
                    ];
                  };
                };
              };
            };
        };
      });
    in result);
})
