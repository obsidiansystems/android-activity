{}:
let
  enableProfiling = false;
in rec {
  tryReflex = import ./try-reflex {};
  nixpkgs = tryReflex.nixpkgs;
  pkgs = tryReflex.nixpkgs;
  inherit (nixpkgs) stdenv;
  backendHaskellPackagesBase = tryReflex.ghc;
  frontendHaskellPackagesBase = tryReflex.ghcjs;
  myPostgres = nixpkgs.postgresql94; #TODO: shouldn't be exposed
  mkDerivation = 
    { name
    , version
    , backendDepends ? (p: [])
    , backendTools ? (p: [])
    , frontendDepends ? (p: [])
    , frontendTools ? (p: [])
    , commonDepends ? (p: [])
    , commonTools ? (p: [])
    , haskellPackagesOverrides ? self: super: {}
    }:
    let
      # Break recursion
      appName = name;
      appVersion = version;

      sharedOverrides = self: super: (import ./override-shared.nix { inherit nixpkgs; }) self super
        // { focus-core = self.callPackage ./core {};
             focus-js = self.callPackage ./js {};
             reflex-dom = self.callPackage ./reflex-dom {};
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
          focus-backend = self.callPackage ./backend { inherit myPostgres; };
          focus-serve = self.callPackage ./http/serve {};
        };
      }).override { overrides = haskellPackagesOverrides; };
      frontendHaskellPackages = extendFrontendHaskellPackages frontendHaskellPackagesBase;
      backendHaskellPackages = extendBackendHaskellPackages backendHaskellPackagesBase;
      mkAssets = (import ./http/assets.nix { inherit nixpkgs; haskellPackages = backendHaskellPackages; }).mkAssets;

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
          build-depends: ${pkgs.lib.concatStringsSep "," ([ "base" "bytestring" "containers" "time" "transformers" "text" "lens" "aeson" "mtl" "directory" "deepseq" "binary" "async" "vector" ] ++ (if haskellPackages.ghc.isGhcjs or false then [ "ghcjs-base" ] else []) ++ builtins.filter (x: x != null) (builtins.map (x: x.pname or null) depends))}
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
        doHaddock = false;
      });
      mkFrontend = src: haskellPackages: static:
        let frontendCommon = common haskellPackages;
        in haskellPackages.callPackage ({mkDerivation, focus-core, focus-js, ghcjs-dom}:
          mkDerivation (rec {
            pname = "${appName}-frontend";
            version = appVersion;
            license = null;
            inherit src;
            preConfigure = mkPreConfigure haskellPackages pname "frontend" buildDepends;
            preBuild = ''
              ln -sfT ${static} static
            '';
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
          })) {};
      mkGhcjsApp = src: static: pkgs.stdenv.mkDerivation (rec {
        name = "ghcjs-app";
        unminified = mkFrontend src frontendHaskellPackages static;
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
      result =  pkgs.stdenv.mkDerivation (rec {
        name = "${appName}-${appVersion}";
        assets = mkAssets ../static;
        zoneinfo = ./zoneinfo;
        frontendJsexeAssets = mkAssets "${mkGhcjsApp ../frontend ../static}/frontend.jsexe";
        marketing = ../marketing;
        # Give the minification step its own derivation so that backend rebuilds don't redo the minification
        frontend = mkGhcjsApp ../frontend ../static;
        builder = builtins.toFile "builder.sh" ''
          source "$stdenv/setup"

          mkdir -p "$out"
          ln -s "$assets" "$out/assets"
          ln -s "$marketing" "$out/marketing"
          ln -s "$backend/bin/backend" "$out"
          ln -s "$frontendJsexeAssets" "$out/frontend.jsexe.assets"
          ln -s "$zoneinfo" "$out/zoneinfo"
        '';
        backend =
          let
            backendCommon = common backendHaskellPackages;
          in backendHaskellPackages.callPackage ({mkDerivation, vector-algorithms, focus-serve, focus-core, focus-backend}: mkDerivation (rec {
            pname = "${appName}-backend";
            license = null;
            version = appVersion;
            src = ../backend;
            preConfigure = mkPreConfigure backendHaskellPackages pname "backend" buildDepends;
            preBuild = ''
              ln -sfT ${../static} static
            '';
            buildDepends = [
              backendCommon
              vector-algorithms
              focus-core focus-backend focus-serve
            ] ++ backendDepends backendHaskellPackages;
            buildTools = [] ++ backendTools pkgs;
            isExecutable = true;
            configureFlags = [ "--ghc-option=-lgcc_s" ] ++ (if enableProfiling then [ "--enable-executable-profiling" ] else [ ]);
            passthru = {
              common = backendCommon;
              haskellPackages = backendHaskellPackages;
            };
          })) {};
        passthru = {
          frontend = frontend.unminified;
          frontendGhc = mkFrontend ../frontend backendHaskellPackages ../static;
          nixpkgs = pkgs;
          completeServer = { hostName, ssl ? false }: import "${nixpkgs.path}/nixos" {
            system = "x86_64-linux";
            configuration = { config, pkgs, ... }: {
              imports = [ "${nixpkgs.path}/nixos/modules/virtualisation/amazon-image.nix" ];
              services.journald.rateLimitBurst = 0;
              ec2.hvm = true;

              environment.systemPackages = with pkgs; [
                emacs24-nox
                git
                rxvt_unicode.terminfo
                myPostgres
              ];

              networking = {
                inherit hostName;
                firewall.allowedTCPPorts = [
                  80 443
                ];
              };

              services.nginx = {
                enable = true;
                httpConfig = if ssl then ''
                  server {
                    listen 80;
                    return 301 https://$host$request_uri;
                  }
                  server {
                    listen 443 ssl;
                    ssl_certificate /var/lib/backend/backend.crt;
                    ssl_certificate_key /var/lib/backend/backend.key;
                    ssl_protocols  TLSv1 TLSv1.1 TLSv1.2;  # don’t use SSLv3 ref: POODLE
                    location / {
                      proxy_pass http://127.0.0.1:8000;
                      proxy_set_header Host $http_host;
                      proxy_set_header X-Forwarded-Proto "https";
                      proxy_read_timeout 300s;
                    }
                    access_log off;
                  }
                '' else ''
                  server {
                    listen 80;
                    location / {
                      proxy_pass http://127.0.0.1:8000;
                      proxy_set_header Host $http_host;
                      proxy_read_timeout 300s;
                    }
                    access_log off;
                  }
                '';
              };

              systemd.services.backend = {
                wantedBy = [ "multi-user.target" ];
                after = [ "network.target" ];
                restartIfChanged = true;
                script = ''
                  export HOME=/var/lib/backend
                  mkdir -p "$HOME"
                  cd "$HOME"
                  pwd
                  ln -sft . "${result}"/*
                  mkdir -p log
                  exec ./backend >>backend.out 2>>backend.err </dev/null
                '';
                serviceConfig = {
                  User = "backend";
                  KillMode = "process";
                };
              };
              users.extraUsers.backend = {
                description = "backend server user";
                home = "/var/lib/backend";
                createHome = true;
                isSystemUser = true;
                uid = 500;
                group = "backend";
              };
              users.extraGroups.backend.gid = 500;
            };
          };
        };
      });
    in result;
}
