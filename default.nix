{ enableProfiling ? false
, runWithHeapProfiling ? false
, enableExposeAllUnfoldings ? false
, enableTraceReflexEvents ? false
}:
assert runWithHeapProfiling -> enableProfiling;
let tryReflex = import ./reflex-platform {
      inherit enableExposeAllUnfoldings enableTraceReflexEvents;
      enableLibraryProfiling = enableProfiling;
      useReflexOptimizer = false;
    };
    inherit (tryReflex) nixpkgs cabal2nixResult;
in with nixpkgs.haskell.lib;
rec {
  inherit tryReflex nixpkgs cabal2nixResult;
  pkgs = tryReflex.nixpkgs;
  inherit (nixpkgs) stdenv;
  backendHaskellPackagesBase = tryReflex.ghc;
  frontendHaskellPackagesBase = tryReflex.ghcjs;
  iosSimulatorHaskellPackagesBase = tryReflex.ghcIosSimulator64;
  myPostgres = nixpkgs.postgresql95; #TODO: shouldn't be exposed
  filterGitSource = p: if builtins.pathExists p then builtins.filterSource (path: type: !(builtins.elem (baseNameOf path) [ ".git" "tags" "TAGS" ])) p else null;
  mkDerivation = nixpkgs.lib.makeOverridable (
    { name
    , version
    , androidPackagePrefix ? "systems.obsidian"
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
    , overrideServerConfig ? (args: outputs: x: x)
    }:
    let
      # Break recursion
      appName = name;
      appVersion = version;

      frontendSrc = filterGitSource ../frontend;
      commonSrc = filterGitSource ../common;
      backendSrc = filterGitSource ../backend;
      marketingSrc = filterGitSource ../marketing;
      staticSrc = filterGitSource ../static;
      backendTestsSrc = filterGitSource ../tests/backend;
      webDriverTestsSrc = filterGitSource ../tests/webdriver;

      sharedOverrides = self: super: (import ./override-shared.nix { inherit nixpkgs filterGitSource; }) self super
        // { focus-aeson-orphans = dontHaddock (self.callPackage (cabal2nixResult (filterGitSource ./aeson-orphans)) {});
             focus-core = dontHaddock (self.callPackage (cabal2nixResult (filterGitSource ./core)) {});
             focus-datastructures = dontHaddock (self.callPackage (cabal2nixResult (filterGitSource ./datastructures)) {});
             focus-emojione = dontHaddock (self.callPackage (cabal2nixResult (filterGitSource ./emojione)) {});
             focus-emojione-data = dontHaddock (self.callPackage (cabal2nixResult (filterGitSource ./emojione/data)) {});
             focus-http-th = dontHaddock (self.callPackage (cabal2nixResult (filterGitSource ./http/th)) {});
             focus-js = overrideCabal (self.callPackage (cabal2nixResult (filterGitSource ./js)) {}) (drv: {
               doHaddock = false;
               libraryHaskellDepends = (drv.libraryHaskellDepends or []) ++ (if self.ghc.isGhcjs or false then (with self; [ghcjs-base ghcjs-json]) else []);
             });
             focus-serve = dontHaddock (self.callPackage (cabal2nixResult (filterGitSource ./http/serve)) {});
             focus-th = dontHaddock (self.callPackage (cabal2nixResult (filterGitSource ./th)) {});
             focus-webdriver = dontHaddock (self.callPackage (cabal2nixResult (filterGitSource ./webdriver)) {});
             email-parse = dontHaddock (self.callPackage (cabal2nixResult (filterGitSource ./email-parse)) {});
             unique-id = dontHaddock (self.callPackage (cabal2nixResult (filterGitSource ./unique-id)) {});
             hellosign = dontHaddock (self.callPackage (cabal2nixResult (filterGitSource ./hellosign)) {});
             touch = dontHaddock (self.callPackage (cabal2nixResult (filterGitSource ./touch)) {});
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
        overrides = self: super: sharedOverrides self super // {
          focus-backend = dontHaddock (self.callPackage ./backend { inherit myPostgres; });
          focus-client = dontHaddock (self.callPackage ./client {});
          focus-test = dontHaddock (self.callPackage ./test {});
        };
      }).override { overrides = haskellPackagesOverrides; };
      frontendHaskellPackages = extendFrontendHaskellPackages frontendHaskellPackagesBase;
      backendHaskellPackages = extendBackendHaskellPackages backendHaskellPackagesBase;
      iosSimulatorHaskellPackages = iosSimulatorHaskellPackagesBase.override {
        overrides = sharedOverrides;
      };
      mkAssets = (import ./http/assets.nix { inherit nixpkgs; }).mkAssets;

      libraryHeader = ''
        library
          exposed-modules: $(find -L * -name '[A-Z]*.hs' | sed 's/\.hs$//' | grep -vi '^main'$ | tr / . | tr "\n" , | sed 's/,$//')
      '';
      executableHeader = executableName: mainFile: ''
        executable ${executableName}
          main-is: ${if mainFile == null
                     then ''$(ls | grep -i '^\(${executableName}\|main\)\.\(l\|\)hs'$)''
                     else mainFile
                    }
      '';
      mkCabalFile = haskellPackages: pname: executableName: depends:
        let mkCabalTarget = header: ''
              ${header}
                hs-source-dirs: .
                build-depends: ${pkgs.lib.concatStringsSep "," ([ "base" "bytestring" "containers" "time" "transformers" "text" "lens" "aeson" "mtl" "directory" "deepseq" "binary" "async" "vector" "template-haskell" "filepath" "primitive" "ghc-prim" ] ++ (if haskellPackages.ghc.isGhcjs or false then [ "ghcjs-base" "ghcjs-prim" ] else [ "process" "unix"]) ++ builtins.filter (x: x != null) (builtins.map (x: x.pname or null) depends))}
                other-extensions: TemplateHaskell
                ghc-options: -threaded -Wall -fwarn-tabs -fno-warn-unused-do-bind -funbox-strict-fields -O2 -fprof-auto -rtsopts -threaded "-with-rtsopts=-N10 -I0" ${if builtins.any (p: (p.name or "") == "reflex") depends then "-fplugin=Reflex.Optimizer" else ""}
                default-language: Haskell2010
                default-extensions: NoDatatypeContexts, NondecreasingIndentation
                if impl(ghcjs)
                  cpp-options: -DGHCJS_GC_INTERVAL=60000 -DGHCJS_BUSY_YIELD=6 -DGHCJS_SCHED_QUANTUM=5
                  ghcjs-options: -dedupe
                if !os(ios)
                  cpp-options: -DUSE_TEMPLATE_HASKELL
            '';
        in ''
        name: ${pname}
        version: ${appVersion}
        cabal-version: >= 1.2

        ${"" /*mkCabalTarget libraryHeader*/ /* Disabled because nothing was actually building libraries anyhow */}

        ${if executableName != null then mkCabalTarget (executableHeader executableName null) else ""}

        $(for x in $(ls | sed -n 's/\([a-z].*\)\.hs$/\1/p' | grep -vi '^main'$) ; do
            cat <<INNER_EOF
        ${mkCabalTarget (executableHeader "$x" "$x.hs")}
        INNER_EOF
        done
        )
      '';

      #TODO: The list of builtin packages should be in nixpkgs, associated with the compiler
      mkPreConfigure = haskellPackages: pname: executableName: depends: ''
        if ! ls | grep ".*\\.cabal$" ; then
          cat >"${pname}.cabal" <<EOF
        ${mkCabalFile haskellPackages pname executableName depends}
        EOF
        fi
        cat *.cabal
      '';

      mkFrontend = frontendSrc: commonSrc: haskellPackages: static:
        haskellPackages.callPackage ({mkDerivation, focus-core, focus-js, ghcjs-dom}:
          mkDerivation (rec {
            pname = "${appName}-frontend";
            version = appVersion;
            license = null;
            src = nixpkgs.runCommand "frontend-src" {
              buildCommand = ''
                mkdir "$out"
                ${if commonSrc != null then ''ln -s "${commonSrc}"/src/* "$out"/'' else ""}
                ln -s "${frontendSrc}"/src{,-bin}/* "$out"/
              '';
            } "";
            preConfigure = mkPreConfigure haskellPackages pname "frontend" buildDepends;
            preBuild = ''
              ${if static == null then "" else ''ln -sfT ${static} static''}
            '';
            buildDepends = [
              focus-core
              focus-js
              ghcjs-dom
            ] ++ frontendDepends haskellPackages ++ commonDepends haskellPackages;
            buildTools = [] ++ frontendTools pkgs;
            isExecutable = true;
            passthru = {
              inherit haskellPackages;
            };
            doHaddock = false;
          })) {};
      ghcjsApp = pkgs.stdenv.mkDerivation (rec {
        name = "ghcjs-app";
        unminified = mkFrontend frontendSrc commonSrc frontendHaskellPackages staticSrc;
        ghcjsExterns = ./ghcjs.externs.js;
        inherit (pkgs) closurecompiler;
        builder = builtins.toFile "builder.sh" ''
          source "$stdenv/setup"

          mkdir -p "$out"
          cd "$out"
          for x in $(ls "$unminified/bin") ; do
            mkdir "$x"
            pushd "$x"
            ln -s "$unminified/bin/$x/all.js" all.unminified.js
            java -Xmx16800m -jar "$closurecompiler/share/java/compiler.jar" --externs "$ghcjsExterns" -O ADVANCED --create_source_map="all.js.map" --source_map_format=V3 --js_output_file="all.js" all.unminified.js
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
      result =  pkgs.stdenv.mkDerivation (rec {
        name = "${appName}-${appVersion}";
        staticAssets = mkAssets (fixupStatic staticSrc);
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
          ln -s "$androidApp" "$out/android"
          if [ -n "''${emails+x}" ] ; then
            ln -s "$emails" "$out/emails"
          fi
        '';
        androidSrc = import ./android { inherit nixpkgs; name = appName; packagePrefix = androidPackagePrefix;}; # frontend = frontend_.unminified; };
        androidApp = nixpkgs.androidenv.buildApp {
          name = appName;
          src = androidSrc;
          platformVersions = [ "23" ];
          useGoogleAPIs = false;
          useNDK = true;
          release = true;
          keyStore = ./keystore;
          keyAlias = "focus";
          keyStorePassword = "password";
          keyAliasPassword = "password";
        };
        androidEmulate = nixpkgs.androidenv.emulateApp {
          name = appName;
          app = androidApp;
          platformVersion = "23";
          enableGPU = true;
          abiVersion = "x86_64";
          useGoogleAPIs = false;
          package = androidPackagePrefix + "." + appName;
          activity = ".MainActivity";
        };
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

            preConfigure = mkPreConfigure backendHaskellPackages pname "backend" buildDepends;
            preBuild = ''
              ${if staticSrc == null then "" else ''ln -sfT ${staticSrc} static''}
            '';
            buildDepends = [
              vector-algorithms
              focus-core focus-backend focus-serve
            ] ++ backendDepends backendHaskellPackages ++ commonDepends backendHaskellPackages ++ frontendDepends backendHaskellPackages;
            buildTools = [] ++ backendTools pkgs;
            isExecutable = true;
            configureFlags = [ "--ghc-option=-lgcc_s" ] ++ (if enableProfiling then [ "--enable-executable-profiling" ] else [ ]);
            passthru = {
              haskellPackages = backendHaskellPackages;
            };
            doHaddock = false;
          })) {};
        passthru = rec {
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
              preConfigure = mkPreConfigure backendHaskellPackages pname "webdriver-tests" buildDepends;
              preBuild = ''
                ln -sfT ${staticSrc} static
              '';
              buildDepends = [ webdriver focus-webdriver ] ++ webDriverTestDepends backendHaskellPackages;
              buildTools = [] ++ webDriverTestTools pkgs;
              isExecutable = true;
              configureFlags = [ ];
              passthru = {
                haskellPackages = backendHaskellPackages;
              };
              doHaddock = false;
          })) {};
          ${if builtins.pathExists ../tests/webdriver then "run-webdriver-tests" else null} =
            { seleniumHost ? "localhost", seleniumPort ? "4444"}:
            let selenium-server = nixpkgs.selenium-server-standalone;
                chromium = nixpkgs.chromium;
                inherit webdriver-tests;
            in nixpkgs.writeScript "run-webdriver-tests" ''
                 "${passthru.webdriver-tests}/bin/webdriver-tests" "${seleniumHost}" "${seleniumPort}" "${chromium}/bin/chromium"
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

                preConfigure = mkPreConfigure backendHaskellPackages pname "backend-tests" buildDepends;
                preBuild = ''
                  ln -sfT ${staticSrc} static
                '';
                buildDepends = [
                  vector-algorithms
                  focus-core focus-backend focus-client
                ] ++ backendTestDepends backendHaskellPackages ++ commonDepends backendHaskellPackages ++ backendDepends backendHaskellPackages;
                buildTools = [] ++ backendTestTools pkgs;
                isExecutable = true;
                configureFlags = [ "--ghc-option=-lgcc_s" ];
                passthru = {
                  haskellPackages = backendHaskellPackages;
                };
                doHaddock = false;
          })) {};
          frontend = frontend_.unminified;
          inherit staticAssets;
          frontendGhc = mkFrontend frontendSrc commonSrc backendHaskellPackages staticSrc;
          frontendIosSimulator = mkFrontend frontendSrc commonSrc iosSimulatorHaskellPackages staticSrc;
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
                           , hasWwwSubdomain ? countCharsWhere (c: c == ".") hostName == 1 # We assume that domains of the form "abc.xyz" will also have a www subdomain, but domains with more components will not
                           }:
            let acmeWebRoot = "/srv/acme/";
                nginxService = {locations}:
                  let locationConfig = path: port: ''
                        location ${path} {
                          ${if path != "/" then "rewrite ^${path}(.*)$ /$1 break;" else ""}
                          proxy_pass http://127.0.0.1:${builtins.toString port};
                          proxy_set_header Host $http_host;
                          proxy_read_timeout 300s;
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
                      }
                      error_log  /var/log/nginx_error.log  warn;
                    '' else ''
                      server {
                        listen 80;
                        ${locationConfigs}
                        access_log off;
                      }
                      error_log  /var/log/nginx_error.log  warn;
                    '';
                  };
                system = "x86_64-linux";
                configuration = args@{ config, pkgs, ... }: overrideServerConfig args { inherit defaultBackendPort defaultBackendUid defaultBackendGid frontend backend backendService nginxService; } {
                  nixpkgs.config.packageOverrides = self: {
                    simp_le =
                      let newNixpkgs = pkgs.fetchFromGitHub {
                            owner = "NixOS";
                            repo = "nixpkgs";
                            rev = "8341cfb64836d4da669f37bd0b8d7ea828cf47c8";
                            sha256 = "00lcmgfhzjyfv27ixlsx9x9vh3q1p9j5d2zhs5cxxigzd9km349x";
                          };
                      in self.callPackage (newNixpkgs + "/pkgs/tools/admin/simp_le") {};
                  };
                  environment.systemPackages = with pkgs; [
                    rsync
                    emacs24-nox
                    git
                    rxvt_unicode.terminfo
                    myPostgres
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
                      exec ${nixpkgs.redir}/bin/redir --lport=25 --laddr=0.0.0.0 --cport=2525
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
                    extraDomains = if hasWwwSubdomain then {
                      "www.${hostName}" = null;
                    } else {};
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
}
