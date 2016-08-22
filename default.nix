{}:
let enableProfiling = false;
    tryReflex = import ./reflex-platform { enableLibraryProfiling = enableProfiling; };
    inherit (tryReflex) nixpkgs cabal2nixResult;
in with nixpkgs.haskell.lib;
rec {
  inherit tryReflex nixpkgs cabal2nixResult;
  pkgs = tryReflex.nixpkgs;
  inherit (nixpkgs) stdenv;
  backendHaskellPackagesBase = tryReflex.ghc;
  frontendHaskellPackagesBase = tryReflex.ghcjs;
  myPostgres = nixpkgs.postgresql95; #TODO: shouldn't be exposed
  filterGitSource = builtins.filterSource (path: type: !(builtins.elem (baseNameOf path) [ ".git" "tags" "TAGS" ]));
  mkDerivation =
    { name
    , version
    , androidPackagePrefix ? "systems.obsidian"
    , backendDepends ? (p: [])
    , backendTools ? (p: [])
    , frontendDepends ? (p: [])
    , frontendTools ? (p: [])
    , testDepends ? (p: [])
    , testTools ? (p : [])
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
      testsSrc = filterGitSource ../tests;

      sharedOverrides = self: super: (import ./override-shared.nix { inherit nixpkgs; }) self super
        // { focus-core = dontHaddock (self.callPackage (cabal2nixResult (filterGitSource ./core)) {});
             focus-emojione = dontHaddock (self.callPackage (cabal2nixResult (filterGitSource ./emojione)) {});
             focus-emojione-data = dontHaddock (self.callPackage (cabal2nixResult (filterGitSource ./emojione/data)) {});
             focus-http-th = dontHaddock (self.callPackage (cabal2nixResult (filterGitSource ./http/th)) {});
             focus-js = dontHaddock (self.callPackage (cabal2nixResult (filterGitSource ./js)) {});
             focus-serve = dontHaddock (self.callPackage (cabal2nixResult (filterGitSource ./http/serve)) {});
             focus-th = dontHaddock (self.callPackage (cabal2nixResult (filterGitSource ./th)) {});
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
                build-depends: ${pkgs.lib.concatStringsSep "," ([ "base" "bytestring" "containers" "time" "transformers" "text" "lens" "aeson" "mtl" "directory" "deepseq" "binary" "async" "vector" "template-haskell" "filepath" ] ++ (if haskellPackages.ghc.isGhcjs or false then [ "ghcjs-prim" ] else [ "process" ]) ++ builtins.filter (x: x != null) (builtins.map (x: x.pname or null) depends))}
                other-extensions: TemplateHaskell
                ghc-options: -threaded -Wall -fwarn-tabs -fno-warn-unused-do-bind -funbox-strict-fields -O2 -fprof-auto-calls -rtsopts -threaded "-with-rtsopts=-N10 -I0"
                if impl(ghcjs)
                  cpp-options: -DGHCJS_GC_INTERVAL=60000
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
                ln -s "${commonSrc}"/src/* "$out"/
                ln -s "${frontendSrc}"/src/* "$out"/
              '';
            } "";
            preConfigure = mkPreConfigure haskellPackages pname "frontend" buildDepends;
            preBuild = ''
              ln -sfT ${static} static
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
        builder = builtins.toFile "builder.sh" ''
          source "$stdenv/setup"

          mkdir -p "$out/frontend.jsexe"
          cd "$out/frontend.jsexe"
          ln -s "$unminified/bin/frontend.jsexe/all.js" all.unminified.js
          closure-compiler -O ADVANCED --create_source_map="all.js.map" --source_map_format=V3 --js_output_file="all.js" all.unminified.js
          echo "//# sourceMappingURL=all.js.map" >> all.js
        '';
        buildInputs = with pkgs; [
          closurecompiler
        ];
        passthru = {
          frontend = unminified;
        };
      });
      result =  pkgs.stdenv.mkDerivation (rec {
        name = "${appName}-${appVersion}";
        assets = mkAssets (fixupStatic staticSrc);
        zoneinfo = ./zoneinfo;
        frontendJsexeAssets = mkAssets "${ghcjsApp}/frontend.jsexe";
        ${if builtins.pathExists ../marketing then "marketing" else null} = marketingSrc;
        # Give the minification step its own derivation so that backend rebuilds don't redo the minification
        frontend = ghcjsApp;
        frontend_ = frontend;
        emails = (import ./emails {}).build;
        builder = builtins.toFile "builder.sh" ''
          source "$stdenv/setup"

          mkdir -p "$out"
          ln -s "$assets" "$out/assets"
          if ! [ -z "''${marketing+x}" ] ; then
            ln -s "$marketing" "$out/marketing"
          fi
          ln -s "$backend/bin/"* "$out"
          ln -s "$frontendJsexeAssets" "$out/frontend.jsexe.assets"
          ln -s "$zoneinfo" "$out/zoneinfo"
          # ln -s "$androidApp" "$out/android"
          ln -s "$emails" "$out/emails"
        '';
        # androidSrc = import ./android { inherit nixpkgs; name = appName; packagePrefix = androidPackagePrefix; frontend = frontend_.unminified; };
        # androidApp = nixpkgs.androidenv.buildApp {
        #   name = appName;
        #   src = androidSrc;
        #   platformVersions = [ "23" ];
        #   useGoogleAPIs = false;
        #   release = true;
        #   keyStore = ./keystore;
        #   keyAlias = "focus";
        #   keyStorePassword = "password";
        #   keyAliasPassword = "password";
        # };
        # androidEmulate = nixpkgs.androidenv.emulateApp {
        #   name = appName;
        #   app = androidApp;
        #   platformVersion = "23";
        #   enableGPU = true;
        #   abiVersion = "x86_64";
        #   useGoogleAPIs = false;
        #   package = androidPackagePrefix + "." + appName;
        #   activity = ".MainActivity";
        # };
        backend =
          backendHaskellPackages.callPackage ({mkDerivation, vector-algorithms, focus-serve, focus-core, focus-backend}: mkDerivation (rec {
            pname = "${appName}-backend";
            license = null;
            version = appVersion;
            src = nixpkgs.runCommand "backend-src" {
              buildCommand = ''
                mkdir "$out"
                ln -s "${commonSrc}"/src/* "$out"/
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
              ln -sfT ${staticSrc} static
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
          ${if builtins.pathExists ../tests then "tests" else null} =
            backendHaskellPackages.callPackage ({mkDerivation, vector-algorithms, focus-core, focus-client, focus-backend}: mkDerivation (rec {
                pname = "${appName}-tests";
                license = null;
                version = appVersion;
                src = testsSrc;
                preConfigure = mkPreConfigure backendHaskellPackages pname "tests" buildDepends;
                preBuild = ''
                  ln -sfT ${staticSrc} static
                '';
                buildDepends = [
                  vector-algorithms
                  focus-core focus-backend focus-client
                ] ++ testDepends backendHaskellPackages;
                buildTools = [] ++ testTools pkgs;
                isExecutable = true;
                configureFlags = [ "--ghc-option=-lgcc_s" ];
                passthru = {
                  haskellPackages = backendHaskellPackages;
                };
                doHaddock = false;
          })) {};
          frontend = frontend_.unminified;
          frontendGhc = mkFrontend frontendSrc commonSrc backendHaskellPackages staticSrc;
          nixpkgs = pkgs;
          backendService = {user, port}: {
            wantedBy = [ "multi-user.target" ];
            after = [ "network.target" ];
            restartIfChanged = true;
            script = ''
              ln -sft . "${result}"/*
              mkdir -p log
              exec ./backend -p "${builtins.toString port}" >>backend.out 2>>backend.err </dev/null
            '';
            serviceConfig = {
              User = user;
              KillMode = "process";
              WorkingDirectory = "~";
            };
          };
          defaultBackendPort = 8000;
          defaultBackendUid = 500;
          defaultBackendGid = 500;
          qemuInfoBlockDelimiter = "GUEST INFO";
          completeServer = { hostName, ssl ? false, adminEmail ? "ops@obsidian.systems", extraRootKey ? null }:
            let acmeWebRoot = "/srv/acme/";
                nginxService = {locations}:
                  let locationConfig = path: port: ''
                        location ${path} {
                          ${if path != "/" then "rewrite ^${path}(.*)$ /$1 break;" else ""}
                          proxy_pass http://127.0.0.1:${builtins.toString port};
                          proxy_set_header Host $http_host;
                          proxy_read_timeout 300s;
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
                  environment.systemPackages = with pkgs; [
                    emacs24-nox
                    git
                    rxvt_unicode.terminfo
                    myPostgres
                  ];

                  networking = {
                    inherit hostName;
                    firewall.allowedTCPPorts = [
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
                    extraDomains = {
                      "www.${hostName}" = null;
                    };
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
    in result;
}
