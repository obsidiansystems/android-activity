{ nixpkgs ? (import ./try-reflex {}).nixpkgs }:

rec {
  zopfli = format: file: nixpkgs.stdenv.mkDerivation {
    name = "zopfli-${format}";
    input = file;
    builder = builtins.toFile "builder.sh" ''
      source "$stdenv/setup"

      zopfli -c --i5 --"${format}" "$input" >"$out"
    '';
    buildInputs = [
      nixpkgs.zopfli
    ];
  };

  encodings = file: nixpkgs.stdenv.mkDerivation {
    name = "encodings";

    input = file;
    gzip = zopfli "gzip" file;
    zlib = zopfli "zlib" file;
    deflate = zopfli "deflate" file;

    builder = builtins.toFile "builder.sh" ''
      source "$stdenv/setup"

      mkdir -p "$out"

      ln -s "$input" "$out/identity"
      ln -s "$gzip" "$out/gzip"
      ln -s "$zlib" "$out/compress"
      ln -s "$deflate" "$out/deflate"
    '';
  };

  mapFiles = f: mapFilesWithName ({name, value}: {
    inherit name;
    value = {
      inherit (value) type;
      path = f value.path;
    };
  });

  mapFilesWithName = f: unionMapFilesWithName (x: { ${x.name} = f x; });

  setToList = s: map (name: { inherit name; value = s.${name}; }) (builtins.attrNames s);

  unionMapFilesWithName = f: d:
    let go = {name, value}:
          if value.type == "directory" then [{
            inherit name;
            value = {
              inherit (value) type;
              contents = unionMapFilesWithName f value.contents;
            };
          }] else setToList (f {
            inherit name value;
          });
    in builtins.listToAttrs (builtins.concatLists (map go (setToList d)));

  readDirRecursive = dir:
    let d = builtins.readDir dir;
        go = name:
          let path = dir + "/${name}";
              type = d.${name};
          in if type == "directory" then {
            inherit name;
            value = {
              inherit type;
              contents = readDirRecursive path;
            };
          } else {
            inherit name;
            value = {
              inherit type path;
            };
          };
    in builtins.listToAttrs (map go (builtins.attrNames d));

  doubleQuoteString = s: "\"" + builtins.replaceStrings ["\\" "\""] ["\\\\" "\\\""] s + "\"";

  dirToPath = contents: toPath {
    type = "directory";
    inherit contents;
  };

  toPath = x:
    if x.type == "directory" then nixpkgs.stdenv.mkDerivation {
      name = "toPath";
      files = builtins.concatStringsSep " " (map (n: "[" + doubleQuoteString n + "]=" + doubleQuoteString (toString (toPath x.contents.${n}))) (builtins.attrNames x.contents));
      builder = builtins.toFile "builder.sh" ''
        source "$stdenv/setup"

        eval "declare -A files=($files)"

        mkdir "$out"

        for filename in "''${!files[@]}" ; do
          ln -s "''${files[$filename]}" "$out/$filename"
        done
      '';
    } else x.path;

  hashFile = path: builtins.readFile (nixpkgs.runCommand "hashFile" {
    buildInputs = [
      nixpkgs.nix
    ];
    inherit path;
  } ''
    nix-hash --flat --base32 --type sha256 "$path" | tr -d '\n' >"$out"
  '');

  dir = contents: {
    type = "directory";
    inherit contents;
  };

  symlink = path: {
    type = "symlink";
    inherit path;
  };

  mkAsset = {name, value}:
    let nameWithHash = "${hashFile value.path}-${name}";
    in {
      ${nameWithHash} = dir {
        type = symlink (builtins.toFile "type" "immutable");
        encodings = symlink (encodings value.path);
      };
      ${name} = dir {
        type = symlink (builtins.toFile "type" "redirect");
        target = symlink (builtins.toFile "target" "${nameWithHash}");
      };
    };

  mkAssets = d: dirToPath (unionMapFilesWithName mkAsset (readDirRecursive d));
}
