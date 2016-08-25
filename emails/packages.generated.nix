{ self, fetchurl, fetchgit ? null, lib }:

{
  by-spec."CSSselect"."~0.4.0" =
    self.by-version."CSSselect"."0.4.1";
  by-version."CSSselect"."0.4.1" = self.buildNodePackage {
    name = "CSSselect-0.4.1";
    version = "0.4.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/CSSselect/-/CSSselect-0.4.1.tgz";
      name = "CSSselect-0.4.1.tgz";
      sha1 = "f8ab7e1f8418ce63cda6eb7bd778a85d7ec492b2";
    };
    deps = {
      "CSSwhat-0.4.7" = self.by-version."CSSwhat"."0.4.7";
      "domutils-1.4.3" = self.by-version."domutils"."1.4.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."CSSwhat"."0.4" =
    self.by-version."CSSwhat"."0.4.7";
  by-version."CSSwhat"."0.4.7" = self.buildNodePackage {
    name = "CSSwhat-0.4.7";
    version = "0.4.7";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/CSSwhat/-/CSSwhat-0.4.7.tgz";
      name = "CSSwhat-0.4.7.tgz";
      sha1 = "867da0ff39f778613242c44cfea83f0aa4ebdf9b";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."abab"."^1.0.0" =
    self.by-version."abab"."1.0.3";
  by-version."abab"."1.0.3" = self.buildNodePackage {
    name = "abab-1.0.3";
    version = "1.0.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/abab/-/abab-1.0.3.tgz";
      name = "abab-1.0.3.tgz";
      sha1 = "b81de5f7274ec4e756d797cd834f303642724e5d";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."abbrev"."1" =
    self.by-version."abbrev"."1.0.9";
  by-version."abbrev"."1.0.9" = self.buildNodePackage {
    name = "abbrev-1.0.9";
    version = "1.0.9";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/abbrev/-/abbrev-1.0.9.tgz";
      name = "abbrev-1.0.9.tgz";
      sha1 = "91b4792588a7738c25f35dd6f63752a2f8776135";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."accepts"."1.1.4" =
    self.by-version."accepts"."1.1.4";
  by-version."accepts"."1.1.4" = self.buildNodePackage {
    name = "accepts-1.1.4";
    version = "1.1.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/accepts/-/accepts-1.1.4.tgz";
      name = "accepts-1.1.4.tgz";
      sha1 = "d71c96f7d41d0feda2c38cd14e8a27c04158df4a";
    };
    deps = {
      "mime-types-2.0.14" = self.by-version."mime-types"."2.0.14";
      "negotiator-0.4.9" = self.by-version."negotiator"."0.4.9";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."accepts"."~1.3.3" =
    self.by-version."accepts"."1.3.3";
  by-version."accepts"."1.3.3" = self.buildNodePackage {
    name = "accepts-1.3.3";
    version = "1.3.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/accepts/-/accepts-1.3.3.tgz";
      name = "accepts-1.3.3.tgz";
      sha1 = "c3ca7434938648c3e0d9c1e328dd68b622c284ca";
    };
    deps = {
      "mime-types-2.1.11" = self.by-version."mime-types"."2.1.11";
      "negotiator-0.6.1" = self.by-version."negotiator"."0.6.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."acorn"."^2.1.0" =
    self.by-version."acorn"."2.7.0";
  by-version."acorn"."2.7.0" = self.buildNodePackage {
    name = "acorn-2.7.0";
    version = "2.7.0";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/acorn/-/acorn-2.7.0.tgz";
      name = "acorn-2.7.0.tgz";
      sha1 = "ab6e7d9d886aaca8b085bc3312b79a198433f0e7";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."acorn"."^2.4.0" =
    self.by-version."acorn"."2.7.0";
  by-spec."acorn-globals"."^1.0.4" =
    self.by-version."acorn-globals"."1.0.9";
  by-version."acorn-globals"."1.0.9" = self.buildNodePackage {
    name = "acorn-globals-1.0.9";
    version = "1.0.9";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/acorn-globals/-/acorn-globals-1.0.9.tgz";
      name = "acorn-globals-1.0.9.tgz";
      sha1 = "55bb5e98691507b74579d0513413217c380c54cf";
    };
    deps = {
      "acorn-2.7.0" = self.by-version."acorn"."2.7.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."addressparser"."~0.3.2" =
    self.by-version."addressparser"."0.3.2";
  by-version."addressparser"."0.3.2" = self.buildNodePackage {
    name = "addressparser-0.3.2";
    version = "0.3.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/addressparser/-/addressparser-0.3.2.tgz";
      name = "addressparser-0.3.2.tgz";
      sha1 = "59873f35e8fcf6c7361c10239261d76e15348bb2";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."after"."0.8.1" =
    self.by-version."after"."0.8.1";
  by-version."after"."0.8.1" = self.buildNodePackage {
    name = "after-0.8.1";
    version = "0.8.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/after/-/after-0.8.1.tgz";
      name = "after-0.8.1.tgz";
      sha1 = "ab5d4fb883f596816d3515f8f791c0af486dd627";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."align-text"."^0.1.1" =
    self.by-version."align-text"."0.1.4";
  by-version."align-text"."0.1.4" = self.buildNodePackage {
    name = "align-text-0.1.4";
    version = "0.1.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/align-text/-/align-text-0.1.4.tgz";
      name = "align-text-0.1.4.tgz";
      sha1 = "0cd90a561093f35d0a99256c22b7069433fad117";
    };
    deps = {
      "kind-of-3.0.4" = self.by-version."kind-of"."3.0.4";
      "longest-1.0.1" = self.by-version."longest"."1.0.1";
      "repeat-string-1.5.4" = self.by-version."repeat-string"."1.5.4";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."align-text"."^0.1.3" =
    self.by-version."align-text"."0.1.4";
  by-spec."amdefine".">=0.0.4" =
    self.by-version."amdefine"."1.0.0";
  by-version."amdefine"."1.0.0" = self.buildNodePackage {
    name = "amdefine-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/amdefine/-/amdefine-1.0.0.tgz";
      name = "amdefine-1.0.0.tgz";
      sha1 = "fd17474700cb5cc9c2b709f0be9d23ce3c198c33";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."ansi-regex"."^0.2.0" =
    self.by-version."ansi-regex"."0.2.1";
  by-version."ansi-regex"."0.2.1" = self.buildNodePackage {
    name = "ansi-regex-0.2.1";
    version = "0.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/ansi-regex/-/ansi-regex-0.2.1.tgz";
      name = "ansi-regex-0.2.1.tgz";
      sha1 = "0d8e946967a3d8143f93e24e298525fc1b2235f9";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."ansi-regex"."^0.2.1" =
    self.by-version."ansi-regex"."0.2.1";
  by-spec."ansi-regex"."^2.0.0" =
    self.by-version."ansi-regex"."2.0.0";
  by-version."ansi-regex"."2.0.0" = self.buildNodePackage {
    name = "ansi-regex-2.0.0";
    version = "2.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/ansi-regex/-/ansi-regex-2.0.0.tgz";
      name = "ansi-regex-2.0.0.tgz";
      sha1 = "c5061b6e0ef8a81775e50f5d66151bf6bf371107";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."ansi-styles"."^1.1.0" =
    self.by-version."ansi-styles"."1.1.0";
  by-version."ansi-styles"."1.1.0" = self.buildNodePackage {
    name = "ansi-styles-1.1.0";
    version = "1.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/ansi-styles/-/ansi-styles-1.1.0.tgz";
      name = "ansi-styles-1.1.0.tgz";
      sha1 = "eaecbf66cd706882760b2f4691582b8f55d7a7de";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."ansi-styles"."^2.2.1" =
    self.by-version."ansi-styles"."2.2.1";
  by-version."ansi-styles"."2.2.1" = self.buildNodePackage {
    name = "ansi-styles-2.2.1";
    version = "2.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/ansi-styles/-/ansi-styles-2.2.1.tgz";
      name = "ansi-styles-2.2.1.tgz";
      sha1 = "b432dd3358b634cf75e1e4664368240533c1ddbe";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."ansi-styles"."~1.0.0" =
    self.by-version."ansi-styles"."1.0.0";
  by-version."ansi-styles"."1.0.0" = self.buildNodePackage {
    name = "ansi-styles-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/ansi-styles/-/ansi-styles-1.0.0.tgz";
      name = "ansi-styles-1.0.0.tgz";
      sha1 = "cb102df1c56f5123eab8b67cd7b98027a0279178";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."anymatch"."^1.3.0" =
    self.by-version."anymatch"."1.3.0";
  by-version."anymatch"."1.3.0" = self.buildNodePackage {
    name = "anymatch-1.3.0";
    version = "1.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/anymatch/-/anymatch-1.3.0.tgz";
      name = "anymatch-1.3.0.tgz";
      sha1 = "a3e52fa39168c825ff57b0248126ce5a8ff95507";
    };
    deps = {
      "arrify-1.0.1" = self.by-version."arrify"."1.0.1";
      "micromatch-2.3.11" = self.by-version."micromatch"."2.3.11";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."aproba"."^1.0.3" =
    self.by-version."aproba"."1.0.4";
  by-version."aproba"."1.0.4" = self.buildNodePackage {
    name = "aproba-1.0.4";
    version = "1.0.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/aproba/-/aproba-1.0.4.tgz";
      name = "aproba-1.0.4.tgz";
      sha1 = "2713680775e7614c8ba186c065d4e2e52d1072c0";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."archive-type"."^3.0.0" =
    self.by-version."archive-type"."3.2.0";
  by-version."archive-type"."3.2.0" = self.buildNodePackage {
    name = "archive-type-3.2.0";
    version = "3.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/archive-type/-/archive-type-3.2.0.tgz";
      name = "archive-type-3.2.0.tgz";
      sha1 = "9cd9c006957ebe95fadad5bd6098942a813737f6";
    };
    deps = {
      "file-type-3.8.0" = self.by-version."file-type"."3.8.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."archive-type"."^3.0.1" =
    self.by-version."archive-type"."3.2.0";
  by-spec."archy"."^1.0.0" =
    self.by-version."archy"."1.0.0";
  by-version."archy"."1.0.0" = self.buildNodePackage {
    name = "archy-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/archy/-/archy-1.0.0.tgz";
      name = "archy-1.0.0.tgz";
      sha1 = "f9c8c13757cc1dd7bc379ac77b2c62a5c2868c40";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."are-we-there-yet"."~1.1.2" =
    self.by-version."are-we-there-yet"."1.1.2";
  by-version."are-we-there-yet"."1.1.2" = self.buildNodePackage {
    name = "are-we-there-yet-1.1.2";
    version = "1.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/are-we-there-yet/-/are-we-there-yet-1.1.2.tgz";
      name = "are-we-there-yet-1.1.2.tgz";
      sha1 = "80e470e95a084794fe1899262c5667c6e88de1b3";
    };
    deps = {
      "delegates-1.0.0" = self.by-version."delegates"."1.0.0";
      "readable-stream-2.1.5" = self.by-version."readable-stream"."2.1.5";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."argparse"."^1.0.7" =
    self.by-version."argparse"."1.0.7";
  by-version."argparse"."1.0.7" = self.buildNodePackage {
    name = "argparse-1.0.7";
    version = "1.0.7";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/argparse/-/argparse-1.0.7.tgz";
      name = "argparse-1.0.7.tgz";
      sha1 = "c289506480557810f14a8bc62d7a06f63ed7f951";
    };
    deps = {
      "sprintf-js-1.0.3" = self.by-version."sprintf-js"."1.0.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."arr-diff"."^2.0.0" =
    self.by-version."arr-diff"."2.0.0";
  by-version."arr-diff"."2.0.0" = self.buildNodePackage {
    name = "arr-diff-2.0.0";
    version = "2.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/arr-diff/-/arr-diff-2.0.0.tgz";
      name = "arr-diff-2.0.0.tgz";
      sha1 = "8f3b827f955a8bd669697e4a4256ac3ceae356cf";
    };
    deps = {
      "arr-flatten-1.0.1" = self.by-version."arr-flatten"."1.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."arr-flatten"."^1.0.1" =
    self.by-version."arr-flatten"."1.0.1";
  by-version."arr-flatten"."1.0.1" = self.buildNodePackage {
    name = "arr-flatten-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/arr-flatten/-/arr-flatten-1.0.1.tgz";
      name = "arr-flatten-1.0.1.tgz";
      sha1 = "e5ffe54d45e19f32f216e91eb99c8ce892bb604b";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."array-differ"."^1.0.0" =
    self.by-version."array-differ"."1.0.0";
  by-version."array-differ"."1.0.0" = self.buildNodePackage {
    name = "array-differ-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/array-differ/-/array-differ-1.0.0.tgz";
      name = "array-differ-1.0.0.tgz";
      sha1 = "eff52e3758249d33be402b8bb8e564bb2b5d4031";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."array-find-index"."^1.0.1" =
    self.by-version."array-find-index"."1.0.1";
  by-version."array-find-index"."1.0.1" = self.buildNodePackage {
    name = "array-find-index-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/array-find-index/-/array-find-index-1.0.1.tgz";
      name = "array-find-index-1.0.1.tgz";
      sha1 = "0bc25ddac941ec8a496ae258fd4ac188003ef3af";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."array-index"."^1.0.0" =
    self.by-version."array-index"."1.0.0";
  by-version."array-index"."1.0.0" = self.buildNodePackage {
    name = "array-index-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/array-index/-/array-index-1.0.0.tgz";
      name = "array-index-1.0.0.tgz";
      sha1 = "ec56a749ee103e4e08c790b9c353df16055b97f9";
    };
    deps = {
      "debug-2.2.0" = self.by-version."debug"."2.2.0";
      "es6-symbol-3.1.0" = self.by-version."es6-symbol"."3.1.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."array-union"."^1.0.1" =
    self.by-version."array-union"."1.0.2";
  by-version."array-union"."1.0.2" = self.buildNodePackage {
    name = "array-union-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/array-union/-/array-union-1.0.2.tgz";
      name = "array-union-1.0.2.tgz";
      sha1 = "9a34410e4f4e3da23dea375be5be70f24778ec39";
    };
    deps = {
      "array-uniq-1.0.3" = self.by-version."array-uniq"."1.0.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."array-uniq"."^1.0.0" =
    self.by-version."array-uniq"."1.0.3";
  by-version."array-uniq"."1.0.3" = self.buildNodePackage {
    name = "array-uniq-1.0.3";
    version = "1.0.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/array-uniq/-/array-uniq-1.0.3.tgz";
      name = "array-uniq-1.0.3.tgz";
      sha1 = "af6ac877a25cc7f74e058894753858dfdb24fdb6";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."array-uniq"."^1.0.1" =
    self.by-version."array-uniq"."1.0.3";
  by-spec."array-uniq"."^1.0.2" =
    self.by-version."array-uniq"."1.0.3";
  by-spec."array-unique"."^0.2.1" =
    self.by-version."array-unique"."0.2.1";
  by-version."array-unique"."0.2.1" = self.buildNodePackage {
    name = "array-unique-0.2.1";
    version = "0.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/array-unique/-/array-unique-0.2.1.tgz";
      name = "array-unique-0.2.1.tgz";
      sha1 = "a1d97ccafcbc2625cc70fadceb36a50c58b01a53";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."arraybuffer.slice"."0.0.6" =
    self.by-version."arraybuffer.slice"."0.0.6";
  by-version."arraybuffer.slice"."0.0.6" = self.buildNodePackage {
    name = "arraybuffer.slice-0.0.6";
    version = "0.0.6";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/arraybuffer.slice/-/arraybuffer.slice-0.0.6.tgz";
      name = "arraybuffer.slice-0.0.6.tgz";
      sha1 = "f33b2159f0532a3f3107a272c0ccfbd1ad2979ca";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."arrify"."^1.0.0" =
    self.by-version."arrify"."1.0.1";
  by-version."arrify"."1.0.1" = self.buildNodePackage {
    name = "arrify-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/arrify/-/arrify-1.0.1.tgz";
      name = "arrify-1.0.1.tgz";
      sha1 = "898508da2226f380df904728456849c1501a4b0d";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."asn1"."0.1.11" =
    self.by-version."asn1"."0.1.11";
  by-version."asn1"."0.1.11" = self.buildNodePackage {
    name = "asn1-0.1.11";
    version = "0.1.11";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/asn1/-/asn1-0.1.11.tgz";
      name = "asn1-0.1.11.tgz";
      sha1 = "559be18376d08a4ec4dbe80877d27818639b2df7";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."asn1"."~0.2.3" =
    self.by-version."asn1"."0.2.3";
  by-version."asn1"."0.2.3" = self.buildNodePackage {
    name = "asn1-0.2.3";
    version = "0.2.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/asn1/-/asn1-0.2.3.tgz";
      name = "asn1-0.2.3.tgz";
      sha1 = "dac8787713c9966849fc8180777ebe9c1ddf3b86";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."assert-plus"."^0.1.5" =
    self.by-version."assert-plus"."0.1.5";
  by-version."assert-plus"."0.1.5" = self.buildNodePackage {
    name = "assert-plus-0.1.5";
    version = "0.1.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/assert-plus/-/assert-plus-0.1.5.tgz";
      name = "assert-plus-0.1.5.tgz";
      sha1 = "ee74009413002d84cec7219c6ac811812e723160";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."assert-plus"."^0.2.0" =
    self.by-version."assert-plus"."0.2.0";
  by-version."assert-plus"."0.2.0" = self.buildNodePackage {
    name = "assert-plus-0.2.0";
    version = "0.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/assert-plus/-/assert-plus-0.2.0.tgz";
      name = "assert-plus-0.2.0.tgz";
      sha1 = "d74e1b87e7affc0db8aadb7021f3fe48101ab234";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."assert-plus"."^1.0.0" =
    self.by-version."assert-plus"."1.0.0";
  by-version."assert-plus"."1.0.0" = self.buildNodePackage {
    name = "assert-plus-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/assert-plus/-/assert-plus-1.0.0.tgz";
      name = "assert-plus-1.0.0.tgz";
      sha1 = "f12e0f3c5d77b0b1cdd9146942e4e96c1e4dd525";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."async"."0.1.15" =
    self.by-version."async"."0.1.15";
  by-version."async"."0.1.15" = self.buildNodePackage {
    name = "async-0.1.15";
    version = "0.1.15";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/async/-/async-0.1.15.tgz";
      name = "async-0.1.15.tgz";
      sha1 = "2180eaca2cf2a6ca5280d41c0585bec9b3e49bd3";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."async"."^1.4.0" =
    self.by-version."async"."1.5.2";
  by-version."async"."1.5.2" = self.buildNodePackage {
    name = "async-1.5.2";
    version = "1.5.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/async/-/async-1.5.2.tgz";
      name = "async-1.5.2.tgz";
      sha1 = "ec6a61ae56480c0c3cb241c95618e20892f9672a";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."async"."^1.5.2" =
    self.by-version."async"."1.5.2";
  by-spec."async"."~0.2.6" =
    self.by-version."async"."0.2.10";
  by-version."async"."0.2.10" = self.buildNodePackage {
    name = "async-0.2.10";
    version = "0.2.10";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/async/-/async-0.2.10.tgz";
      name = "async-0.2.10.tgz";
      sha1 = "b6bbe0b0674b9d719708ca38de8c237cb526c3d1";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."async"."~0.9.0" =
    self.by-version."async"."0.9.2";
  by-version."async"."0.9.2" = self.buildNodePackage {
    name = "async-0.9.2";
    version = "0.9.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/async/-/async-0.9.2.tgz";
      name = "async-0.9.2.tgz";
      sha1 = "aea74d5e61c1f899613bf64bda66d4c78f2fd17d";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."async-each"."^0.1.6" =
    self.by-version."async-each"."0.1.6";
  by-version."async-each"."0.1.6" = self.buildNodePackage {
    name = "async-each-0.1.6";
    version = "0.1.6";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/async-each/-/async-each-0.1.6.tgz";
      name = "async-each-0.1.6.tgz";
      sha1 = "b67e99edcddf96541e44af56290cd7d5c6e70439";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."async-each-series"."0.1.1" =
    self.by-version."async-each-series"."0.1.1";
  by-version."async-each-series"."0.1.1" = self.buildNodePackage {
    name = "async-each-series-0.1.1";
    version = "0.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/async-each-series/-/async-each-series-0.1.1.tgz";
      name = "async-each-series-0.1.1.tgz";
      sha1 = "7617c1917401fd8ca4a28aadce3dbae98afeb432";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."async-each-series"."^0.1.1" =
    self.by-version."async-each-series"."0.1.1";
  by-spec."async-each-series"."^1.1.0" =
    self.by-version."async-each-series"."1.1.0";
  by-version."async-each-series"."1.1.0" = self.buildNodePackage {
    name = "async-each-series-1.1.0";
    version = "1.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/async-each-series/-/async-each-series-1.1.0.tgz";
      name = "async-each-series-1.1.0.tgz";
      sha1 = "f42fd8155d38f21a5b8ea07c28e063ed1700b138";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."async-foreach"."^0.1.3" =
    self.by-version."async-foreach"."0.1.3";
  by-version."async-foreach"."0.1.3" = self.buildNodePackage {
    name = "async-foreach-0.1.3";
    version = "0.1.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/async-foreach/-/async-foreach-0.1.3.tgz";
      name = "async-foreach-0.1.3.tgz";
      sha1 = "36121f845c0578172de419a97dbeb1d16ec34542";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."atob"."~1.1.0" =
    self.by-version."atob"."1.1.3";
  by-version."atob"."1.1.3" = self.buildNodePackage {
    name = "atob-1.1.3";
    version = "1.1.3";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/atob/-/atob-1.1.3.tgz";
      name = "atob-1.1.3.tgz";
      sha1 = "95f13629b12c3a51a5d215abdce2aa9f32f80773";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."aws-sdk"."2.0.5" =
    self.by-version."aws-sdk"."2.0.5";
  by-version."aws-sdk"."2.0.5" = self.buildNodePackage {
    name = "aws-sdk-2.0.5";
    version = "2.0.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/aws-sdk/-/aws-sdk-2.0.5.tgz";
      name = "aws-sdk-2.0.5.tgz";
      sha1 = "f3ebb1898d0632b7b6672e8d77728cbbb69f98c6";
    };
    deps = {
      "aws-sdk-apis-3.1.10" = self.by-version."aws-sdk-apis"."3.1.10";
      "xml2js-0.2.6" = self.by-version."xml2js"."0.2.6";
      "xmlbuilder-0.4.2" = self.by-version."xmlbuilder"."0.4.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."aws-sdk"."^2.1.16" =
    self.by-version."aws-sdk"."2.5.1";
  by-version."aws-sdk"."2.5.1" = self.buildNodePackage {
    name = "aws-sdk-2.5.1";
    version = "2.5.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/aws-sdk/-/aws-sdk-2.5.1.tgz";
      name = "aws-sdk-2.5.1.tgz";
      sha1 = "2ad14989eb63b2579bef35691c4d7027b03b580a";
    };
    deps = {
      "sax-1.1.5" = self.by-version."sax"."1.1.5";
      "xml2js-0.4.15" = self.by-version."xml2js"."0.4.15";
      "xmlbuilder-2.6.2" = self.by-version."xmlbuilder"."2.6.2";
      "jmespath-0.15.0" = self.by-version."jmespath"."0.15.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."aws-sdk-apis"."3.x" =
    self.by-version."aws-sdk-apis"."3.1.10";
  by-version."aws-sdk-apis"."3.1.10" = self.buildNodePackage {
    name = "aws-sdk-apis-3.1.10";
    version = "3.1.10";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/aws-sdk-apis/-/aws-sdk-apis-3.1.10.tgz";
      name = "aws-sdk-apis-3.1.10.tgz";
      sha1 = "4eed97f590a16cf080fd1b8d8cfdf2472de8ab0e";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."aws-sign2"."~0.5.0" =
    self.by-version."aws-sign2"."0.5.0";
  by-version."aws-sign2"."0.5.0" = self.buildNodePackage {
    name = "aws-sign2-0.5.0";
    version = "0.5.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/aws-sign2/-/aws-sign2-0.5.0.tgz";
      name = "aws-sign2-0.5.0.tgz";
      sha1 = "c57103f7a17fc037f02d7c2e64b602ea223f7d63";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."aws-sign2"."~0.6.0" =
    self.by-version."aws-sign2"."0.6.0";
  by-version."aws-sign2"."0.6.0" = self.buildNodePackage {
    name = "aws-sign2-0.6.0";
    version = "0.6.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/aws-sign2/-/aws-sign2-0.6.0.tgz";
      name = "aws-sign2-0.6.0.tgz";
      sha1 = "14342dd38dbcc94d0e5b87d763cd63612c0e794f";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."aws4"."^1.2.1" =
    self.by-version."aws4"."1.4.1";
  by-version."aws4"."1.4.1" = self.buildNodePackage {
    name = "aws4-1.4.1";
    version = "1.4.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/aws4/-/aws4-1.4.1.tgz";
      name = "aws4-1.4.1.tgz";
      sha1 = "fde7d5292466d230e5ee0f4e038d9dfaab08fc61";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-code-frame"."^6.3.13" =
    self.by-version."babel-code-frame"."6.11.0";
  by-version."babel-code-frame"."6.11.0" = self.buildNodePackage {
    name = "babel-code-frame-6.11.0";
    version = "6.11.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-code-frame/-/babel-code-frame-6.11.0.tgz";
      name = "babel-code-frame-6.11.0.tgz";
      sha1 = "9072dd2353fb0f85b6b57d2c97f0d134d188aed8";
    };
    deps = {
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
      "chalk-1.1.3" = self.by-version."chalk"."1.1.3";
      "esutils-2.0.2" = self.by-version."esutils"."2.0.2";
      "js-tokens-2.0.0" = self.by-version."js-tokens"."2.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-code-frame"."^6.8.0" =
    self.by-version."babel-code-frame"."6.11.0";
  by-spec."babel-core"."^6.11.4" =
    self.by-version."babel-core"."6.13.2";
  by-version."babel-core"."6.13.2" = self.buildNodePackage {
    name = "babel-core-6.13.2";
    version = "6.13.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-core/-/babel-core-6.13.2.tgz";
      name = "babel-core-6.13.2.tgz";
      sha1 = "f761e1199361d5a6ed16f93ce801ad50acadb338";
    };
    deps = {
      "babel-code-frame-6.11.0" = self.by-version."babel-code-frame"."6.11.0";
      "babel-generator-6.11.4" = self.by-version."babel-generator"."6.11.4";
      "babel-helpers-6.8.0" = self.by-version."babel-helpers"."6.8.0";
      "babel-messages-6.8.0" = self.by-version."babel-messages"."6.8.0";
      "babel-template-6.9.0" = self.by-version."babel-template"."6.9.0";
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
      "babel-register-6.11.6" = self.by-version."babel-register"."6.11.6";
      "babel-traverse-6.13.0" = self.by-version."babel-traverse"."6.13.0";
      "babel-types-6.13.0" = self.by-version."babel-types"."6.13.0";
      "babylon-6.9.0" = self.by-version."babylon"."6.9.0";
      "convert-source-map-1.3.0" = self.by-version."convert-source-map"."1.3.0";
      "debug-2.2.0" = self.by-version."debug"."2.2.0";
      "json5-0.4.0" = self.by-version."json5"."0.4.0";
      "lodash-4.15.0" = self.by-version."lodash"."4.15.0";
      "minimatch-3.0.3" = self.by-version."minimatch"."3.0.3";
      "path-exists-1.0.0" = self.by-version."path-exists"."1.0.0";
      "path-is-absolute-1.0.0" = self.by-version."path-is-absolute"."1.0.0";
      "private-0.1.6" = self.by-version."private"."0.1.6";
      "shebang-regex-1.0.0" = self.by-version."shebang-regex"."1.0.0";
      "slash-1.0.0" = self.by-version."slash"."1.0.0";
      "source-map-0.5.6" = self.by-version."source-map"."0.5.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-core"."^6.7.2" =
    self.by-version."babel-core"."6.13.2";
  by-spec."babel-core"."^6.9.0" =
    self.by-version."babel-core"."6.13.2";
  by-spec."babel-core"."~6.3.26" =
    self.by-version."babel-core"."6.3.26";
  by-version."babel-core"."6.3.26" = self.buildNodePackage {
    name = "babel-core-6.3.26";
    version = "6.3.26";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-core/-/babel-core-6.3.26.tgz";
      name = "babel-core-6.3.26.tgz";
      sha1 = "1ec9e81a53a9e3e2655225bdff62f28f6c5e224b";
    };
    deps = {
      "babel-code-frame-6.11.0" = self.by-version."babel-code-frame"."6.11.0";
      "babel-generator-6.11.4" = self.by-version."babel-generator"."6.11.4";
      "babel-helpers-6.8.0" = self.by-version."babel-helpers"."6.8.0";
      "babel-messages-6.8.0" = self.by-version."babel-messages"."6.8.0";
      "babel-template-6.9.0" = self.by-version."babel-template"."6.9.0";
      "babel-runtime-5.8.38" = self.by-version."babel-runtime"."5.8.38";
      "babel-register-6.11.6" = self.by-version."babel-register"."6.11.6";
      "babel-traverse-6.13.0" = self.by-version."babel-traverse"."6.13.0";
      "babel-types-6.13.0" = self.by-version."babel-types"."6.13.0";
      "babylon-6.9.0" = self.by-version."babylon"."6.9.0";
      "convert-source-map-1.3.0" = self.by-version."convert-source-map"."1.3.0";
      "debug-2.2.0" = self.by-version."debug"."2.2.0";
      "json5-0.4.0" = self.by-version."json5"."0.4.0";
      "lodash-3.10.1" = self.by-version."lodash"."3.10.1";
      "minimatch-2.0.10" = self.by-version."minimatch"."2.0.10";
      "path-exists-1.0.0" = self.by-version."path-exists"."1.0.0";
      "path-is-absolute-1.0.0" = self.by-version."path-is-absolute"."1.0.0";
      "private-0.1.6" = self.by-version."private"."0.1.6";
      "shebang-regex-1.0.0" = self.by-version."shebang-regex"."1.0.0";
      "slash-1.0.0" = self.by-version."slash"."1.0.0";
      "source-map-0.5.6" = self.by-version."source-map"."0.5.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "babel-core" = self.by-version."babel-core"."6.3.26";
  by-spec."babel-generator"."^6.11.4" =
    self.by-version."babel-generator"."6.11.4";
  by-version."babel-generator"."6.11.4" = self.buildNodePackage {
    name = "babel-generator-6.11.4";
    version = "6.11.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-generator/-/babel-generator-6.11.4.tgz";
      name = "babel-generator-6.11.4.tgz";
      sha1 = "14f6933abb20c62666d27e3b7b9f5b9dc0712a9a";
    };
    deps = {
      "babel-messages-6.8.0" = self.by-version."babel-messages"."6.8.0";
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
      "babel-types-6.13.0" = self.by-version."babel-types"."6.13.0";
      "detect-indent-3.0.1" = self.by-version."detect-indent"."3.0.1";
      "lodash-4.15.0" = self.by-version."lodash"."4.15.0";
      "source-map-0.5.6" = self.by-version."source-map"."0.5.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-generator"."^6.3.26" =
    self.by-version."babel-generator"."6.11.4";
  by-spec."babel-helper-call-delegate"."^6.8.0" =
    self.by-version."babel-helper-call-delegate"."6.8.0";
  by-version."babel-helper-call-delegate"."6.8.0" = self.buildNodePackage {
    name = "babel-helper-call-delegate-6.8.0";
    version = "6.8.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-helper-call-delegate/-/babel-helper-call-delegate-6.8.0.tgz";
      name = "babel-helper-call-delegate-6.8.0.tgz";
      sha1 = "9d283e7486779b6b0481864a11b371ea5c01fa64";
    };
    deps = {
      "babel-traverse-6.13.0" = self.by-version."babel-traverse"."6.13.0";
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
      "babel-types-6.13.0" = self.by-version."babel-types"."6.13.0";
      "babel-helper-hoist-variables-6.8.0" = self.by-version."babel-helper-hoist-variables"."6.8.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-helper-define-map"."^6.8.0" =
    self.by-version."babel-helper-define-map"."6.9.0";
  by-version."babel-helper-define-map"."6.9.0" = self.buildNodePackage {
    name = "babel-helper-define-map-6.9.0";
    version = "6.9.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-helper-define-map/-/babel-helper-define-map-6.9.0.tgz";
      name = "babel-helper-define-map-6.9.0.tgz";
      sha1 = "6629f9b2a7e58e18e8379a57d1e6fbb2969902fb";
    };
    deps = {
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
      "lodash-4.15.0" = self.by-version."lodash"."4.15.0";
      "babel-types-6.13.0" = self.by-version."babel-types"."6.13.0";
      "babel-helper-function-name-6.8.0" = self.by-version."babel-helper-function-name"."6.8.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-helper-define-map"."^6.9.0" =
    self.by-version."babel-helper-define-map"."6.9.0";
  by-spec."babel-helper-function-name"."^6.8.0" =
    self.by-version."babel-helper-function-name"."6.8.0";
  by-version."babel-helper-function-name"."6.8.0" = self.buildNodePackage {
    name = "babel-helper-function-name-6.8.0";
    version = "6.8.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-helper-function-name/-/babel-helper-function-name-6.8.0.tgz";
      name = "babel-helper-function-name-6.8.0.tgz";
      sha1 = "a0336ba14526a075cdf502fc52d3fe84b12f7a34";
    };
    deps = {
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
      "babel-types-6.13.0" = self.by-version."babel-types"."6.13.0";
      "babel-traverse-6.13.0" = self.by-version."babel-traverse"."6.13.0";
      "babel-helper-get-function-arity-6.8.0" = self.by-version."babel-helper-get-function-arity"."6.8.0";
      "babel-template-6.9.0" = self.by-version."babel-template"."6.9.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-helper-get-function-arity"."^6.8.0" =
    self.by-version."babel-helper-get-function-arity"."6.8.0";
  by-version."babel-helper-get-function-arity"."6.8.0" = self.buildNodePackage {
    name = "babel-helper-get-function-arity-6.8.0";
    version = "6.8.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-helper-get-function-arity/-/babel-helper-get-function-arity-6.8.0.tgz";
      name = "babel-helper-get-function-arity-6.8.0.tgz";
      sha1 = "88276c24bd251cdf6f61b6f89f745f486ced92af";
    };
    deps = {
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
      "babel-types-6.13.0" = self.by-version."babel-types"."6.13.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-helper-hoist-variables"."^6.8.0" =
    self.by-version."babel-helper-hoist-variables"."6.8.0";
  by-version."babel-helper-hoist-variables"."6.8.0" = self.buildNodePackage {
    name = "babel-helper-hoist-variables-6.8.0";
    version = "6.8.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-helper-hoist-variables/-/babel-helper-hoist-variables-6.8.0.tgz";
      name = "babel-helper-hoist-variables-6.8.0.tgz";
      sha1 = "8b0766dc026ea9ea423bc2b34e665a4da7373aaf";
    };
    deps = {
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
      "babel-types-6.13.0" = self.by-version."babel-types"."6.13.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-helper-optimise-call-expression"."^6.8.0" =
    self.by-version."babel-helper-optimise-call-expression"."6.8.0";
  by-version."babel-helper-optimise-call-expression"."6.8.0" = self.buildNodePackage {
    name = "babel-helper-optimise-call-expression-6.8.0";
    version = "6.8.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-helper-optimise-call-expression/-/babel-helper-optimise-call-expression-6.8.0.tgz";
      name = "babel-helper-optimise-call-expression-6.8.0.tgz";
      sha1 = "4175628e9c89fc36174904f27070f29d38567f06";
    };
    deps = {
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
      "babel-types-6.13.0" = self.by-version."babel-types"."6.13.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-helper-regex"."^6.8.0" =
    self.by-version."babel-helper-regex"."6.9.0";
  by-version."babel-helper-regex"."6.9.0" = self.buildNodePackage {
    name = "babel-helper-regex-6.9.0";
    version = "6.9.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-helper-regex/-/babel-helper-regex-6.9.0.tgz";
      name = "babel-helper-regex-6.9.0.tgz";
      sha1 = "c74265fde180ff9a16735fee05e63cadb9e0b057";
    };
    deps = {
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
      "lodash-4.15.0" = self.by-version."lodash"."4.15.0";
      "babel-types-6.13.0" = self.by-version."babel-types"."6.13.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-helper-replace-supers"."^6.8.0" =
    self.by-version."babel-helper-replace-supers"."6.8.0";
  by-version."babel-helper-replace-supers"."6.8.0" = self.buildNodePackage {
    name = "babel-helper-replace-supers-6.8.0";
    version = "6.8.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-helper-replace-supers/-/babel-helper-replace-supers-6.8.0.tgz";
      name = "babel-helper-replace-supers-6.8.0.tgz";
      sha1 = "69cb6bc4ee90164325407af1a2536a42e8fb90d5";
    };
    deps = {
      "babel-helper-optimise-call-expression-6.8.0" = self.by-version."babel-helper-optimise-call-expression"."6.8.0";
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
      "babel-traverse-6.13.0" = self.by-version."babel-traverse"."6.13.0";
      "babel-messages-6.8.0" = self.by-version."babel-messages"."6.8.0";
      "babel-template-6.9.0" = self.by-version."babel-template"."6.9.0";
      "babel-types-6.13.0" = self.by-version."babel-types"."6.13.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-helpers"."^6.3.13" =
    self.by-version."babel-helpers"."6.8.0";
  by-version."babel-helpers"."6.8.0" = self.buildNodePackage {
    name = "babel-helpers-6.8.0";
    version = "6.8.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-helpers/-/babel-helpers-6.8.0.tgz";
      name = "babel-helpers-6.8.0.tgz";
      sha1 = "321c56f9c9cac1a297f827fdff638b27a6292503";
    };
    deps = {
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
      "babel-template-6.9.0" = self.by-version."babel-template"."6.9.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-helpers"."^6.8.0" =
    self.by-version."babel-helpers"."6.8.0";
  by-spec."babel-messages"."^6.3.13" =
    self.by-version."babel-messages"."6.8.0";
  by-version."babel-messages"."6.8.0" = self.buildNodePackage {
    name = "babel-messages-6.8.0";
    version = "6.8.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-messages/-/babel-messages-6.8.0.tgz";
      name = "babel-messages-6.8.0.tgz";
      sha1 = "bf504736ca967e6d65ef0adb5a2a5f947c8e0eb9";
    };
    deps = {
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-messages"."^6.8.0" =
    self.by-version."babel-messages"."6.8.0";
  by-spec."babel-plugin-check-es2015-constants"."^6.3.13" =
    self.by-version."babel-plugin-check-es2015-constants"."6.8.0";
  by-version."babel-plugin-check-es2015-constants"."6.8.0" = self.buildNodePackage {
    name = "babel-plugin-check-es2015-constants-6.8.0";
    version = "6.8.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-plugin-check-es2015-constants/-/babel-plugin-check-es2015-constants-6.8.0.tgz";
      name = "babel-plugin-check-es2015-constants-6.8.0.tgz";
      sha1 = "dbf024c32ed37bfda8dee1e76da02386a8d26fe7";
    };
    deps = {
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-plugin-syntax-async-functions"."^6.8.0" =
    self.by-version."babel-plugin-syntax-async-functions"."6.13.0";
  by-version."babel-plugin-syntax-async-functions"."6.13.0" = self.buildNodePackage {
    name = "babel-plugin-syntax-async-functions-6.13.0";
    version = "6.13.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-plugin-syntax-async-functions/-/babel-plugin-syntax-async-functions-6.13.0.tgz";
      name = "babel-plugin-syntax-async-functions-6.13.0.tgz";
      sha1 = "cad9cad1191b5ad634bf30ae0872391e0647be95";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-plugin-transform-es2015-arrow-functions"."^6.3.13" =
    self.by-version."babel-plugin-transform-es2015-arrow-functions"."6.8.0";
  by-version."babel-plugin-transform-es2015-arrow-functions"."6.8.0" = self.buildNodePackage {
    name = "babel-plugin-transform-es2015-arrow-functions-6.8.0";
    version = "6.8.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-plugin-transform-es2015-arrow-functions/-/babel-plugin-transform-es2015-arrow-functions-6.8.0.tgz";
      name = "babel-plugin-transform-es2015-arrow-functions-6.8.0.tgz";
      sha1 = "5b63afc3181bdc9a8c4d481b5a4f3f7d7fef3d9d";
    };
    deps = {
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-plugin-transform-es2015-block-scoped-functions"."^6.3.13" =
    self.by-version."babel-plugin-transform-es2015-block-scoped-functions"."6.8.0";
  by-version."babel-plugin-transform-es2015-block-scoped-functions"."6.8.0" = self.buildNodePackage {
    name = "babel-plugin-transform-es2015-block-scoped-functions-6.8.0";
    version = "6.8.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-plugin-transform-es2015-block-scoped-functions/-/babel-plugin-transform-es2015-block-scoped-functions-6.8.0.tgz";
      name = "babel-plugin-transform-es2015-block-scoped-functions-6.8.0.tgz";
      sha1 = "ed95d629c4b5a71ae29682b998f70d9833eb366d";
    };
    deps = {
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-plugin-transform-es2015-block-scoping"."^6.3.13" =
    self.by-version."babel-plugin-transform-es2015-block-scoping"."6.10.1";
  by-version."babel-plugin-transform-es2015-block-scoping"."6.10.1" = self.buildNodePackage {
    name = "babel-plugin-transform-es2015-block-scoping-6.10.1";
    version = "6.10.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-plugin-transform-es2015-block-scoping/-/babel-plugin-transform-es2015-block-scoping-6.10.1.tgz";
      name = "babel-plugin-transform-es2015-block-scoping-6.10.1.tgz";
      sha1 = "6ffbe42d08ed14cf889d06e27dc408080f9d5424";
    };
    deps = {
      "babel-traverse-6.13.0" = self.by-version."babel-traverse"."6.13.0";
      "babel-types-6.13.0" = self.by-version."babel-types"."6.13.0";
      "babel-template-6.9.0" = self.by-version."babel-template"."6.9.0";
      "lodash-4.15.0" = self.by-version."lodash"."4.15.0";
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-plugin-transform-es2015-block-scoping"."^6.9.0" =
    self.by-version."babel-plugin-transform-es2015-block-scoping"."6.10.1";
  by-spec."babel-plugin-transform-es2015-classes"."^6.3.13" =
    self.by-version."babel-plugin-transform-es2015-classes"."6.9.0";
  by-version."babel-plugin-transform-es2015-classes"."6.9.0" = self.buildNodePackage {
    name = "babel-plugin-transform-es2015-classes-6.9.0";
    version = "6.9.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-plugin-transform-es2015-classes/-/babel-plugin-transform-es2015-classes-6.9.0.tgz";
      name = "babel-plugin-transform-es2015-classes-6.9.0.tgz";
      sha1 = "2c70aadc2cbb279b99fbc7bccea87177cc8c1df2";
    };
    deps = {
      "babel-helper-optimise-call-expression-6.8.0" = self.by-version."babel-helper-optimise-call-expression"."6.8.0";
      "babel-helper-function-name-6.8.0" = self.by-version."babel-helper-function-name"."6.8.0";
      "babel-helper-replace-supers-6.8.0" = self.by-version."babel-helper-replace-supers"."6.8.0";
      "babel-template-6.9.0" = self.by-version."babel-template"."6.9.0";
      "babel-traverse-6.13.0" = self.by-version."babel-traverse"."6.13.0";
      "babel-helper-define-map-6.9.0" = self.by-version."babel-helper-define-map"."6.9.0";
      "babel-messages-6.8.0" = self.by-version."babel-messages"."6.8.0";
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
      "babel-types-6.13.0" = self.by-version."babel-types"."6.13.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-plugin-transform-es2015-computed-properties"."^6.3.13" =
    self.by-version."babel-plugin-transform-es2015-computed-properties"."6.8.0";
  by-version."babel-plugin-transform-es2015-computed-properties"."6.8.0" = self.buildNodePackage {
    name = "babel-plugin-transform-es2015-computed-properties-6.8.0";
    version = "6.8.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-plugin-transform-es2015-computed-properties/-/babel-plugin-transform-es2015-computed-properties-6.8.0.tgz";
      name = "babel-plugin-transform-es2015-computed-properties-6.8.0.tgz";
      sha1 = "f51010fd61b3bd7b6b60a5fdfd307bb7a5279870";
    };
    deps = {
      "babel-helper-define-map-6.9.0" = self.by-version."babel-helper-define-map"."6.9.0";
      "babel-template-6.9.0" = self.by-version."babel-template"."6.9.0";
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-plugin-transform-es2015-destructuring"."^6.3.13" =
    self.by-version."babel-plugin-transform-es2015-destructuring"."6.9.0";
  by-version."babel-plugin-transform-es2015-destructuring"."6.9.0" = self.buildNodePackage {
    name = "babel-plugin-transform-es2015-destructuring-6.9.0";
    version = "6.9.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-plugin-transform-es2015-destructuring/-/babel-plugin-transform-es2015-destructuring-6.9.0.tgz";
      name = "babel-plugin-transform-es2015-destructuring-6.9.0.tgz";
      sha1 = "f55747f62534866a51b4c4fdb255e6d85e8604d6";
    };
    deps = {
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-plugin-transform-es2015-for-of"."^6.3.13" =
    self.by-version."babel-plugin-transform-es2015-for-of"."6.8.0";
  by-version."babel-plugin-transform-es2015-for-of"."6.8.0" = self.buildNodePackage {
    name = "babel-plugin-transform-es2015-for-of-6.8.0";
    version = "6.8.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-plugin-transform-es2015-for-of/-/babel-plugin-transform-es2015-for-of-6.8.0.tgz";
      name = "babel-plugin-transform-es2015-for-of-6.8.0.tgz";
      sha1 = "82eda139ba4270dda135c3ec1b1f2813fa62f23c";
    };
    deps = {
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-plugin-transform-es2015-for-of"."^6.8.0" =
    self.by-version."babel-plugin-transform-es2015-for-of"."6.8.0";
  by-spec."babel-plugin-transform-es2015-function-name"."^6.3.13" =
    self.by-version."babel-plugin-transform-es2015-function-name"."6.9.0";
  by-version."babel-plugin-transform-es2015-function-name"."6.9.0" = self.buildNodePackage {
    name = "babel-plugin-transform-es2015-function-name-6.9.0";
    version = "6.9.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-plugin-transform-es2015-function-name/-/babel-plugin-transform-es2015-function-name-6.9.0.tgz";
      name = "babel-plugin-transform-es2015-function-name-6.9.0.tgz";
      sha1 = "8c135b17dbd064e5bba56ec511baaee2fca82719";
    };
    deps = {
      "babel-helper-function-name-6.8.0" = self.by-version."babel-helper-function-name"."6.8.0";
      "babel-types-6.13.0" = self.by-version."babel-types"."6.13.0";
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-plugin-transform-es2015-literals"."^6.3.13" =
    self.by-version."babel-plugin-transform-es2015-literals"."6.8.0";
  by-version."babel-plugin-transform-es2015-literals"."6.8.0" = self.buildNodePackage {
    name = "babel-plugin-transform-es2015-literals-6.8.0";
    version = "6.8.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-plugin-transform-es2015-literals/-/babel-plugin-transform-es2015-literals-6.8.0.tgz";
      name = "babel-plugin-transform-es2015-literals-6.8.0.tgz";
      sha1 = "50aa2e5c7958fc2ab25d74ec117e0cc98f046468";
    };
    deps = {
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-plugin-transform-es2015-modules-commonjs"."^6.3.13" =
    self.by-version."babel-plugin-transform-es2015-modules-commonjs"."6.11.5";
  by-version."babel-plugin-transform-es2015-modules-commonjs"."6.11.5" = self.buildNodePackage {
    name = "babel-plugin-transform-es2015-modules-commonjs-6.11.5";
    version = "6.11.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-plugin-transform-es2015-modules-commonjs/-/babel-plugin-transform-es2015-modules-commonjs-6.11.5.tgz";
      name = "babel-plugin-transform-es2015-modules-commonjs-6.11.5.tgz";
      sha1 = "202580c24f286359eade67685bef6e2c6416558a";
    };
    deps = {
      "babel-types-6.13.0" = self.by-version."babel-types"."6.13.0";
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
      "babel-template-6.9.0" = self.by-version."babel-template"."6.9.0";
      "babel-plugin-transform-strict-mode-6.11.3" = self.by-version."babel-plugin-transform-strict-mode"."6.11.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-plugin-transform-es2015-object-super"."^6.3.13" =
    self.by-version."babel-plugin-transform-es2015-object-super"."6.8.0";
  by-version."babel-plugin-transform-es2015-object-super"."6.8.0" = self.buildNodePackage {
    name = "babel-plugin-transform-es2015-object-super-6.8.0";
    version = "6.8.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-plugin-transform-es2015-object-super/-/babel-plugin-transform-es2015-object-super-6.8.0.tgz";
      name = "babel-plugin-transform-es2015-object-super-6.8.0.tgz";
      sha1 = "1b858740a5a4400887c23dcff6f4d56eea4a24c5";
    };
    deps = {
      "babel-helper-replace-supers-6.8.0" = self.by-version."babel-helper-replace-supers"."6.8.0";
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-plugin-transform-es2015-parameters"."^6.3.13" =
    self.by-version."babel-plugin-transform-es2015-parameters"."6.11.4";
  by-version."babel-plugin-transform-es2015-parameters"."6.11.4" = self.buildNodePackage {
    name = "babel-plugin-transform-es2015-parameters-6.11.4";
    version = "6.11.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-plugin-transform-es2015-parameters/-/babel-plugin-transform-es2015-parameters-6.11.4.tgz";
      name = "babel-plugin-transform-es2015-parameters-6.11.4.tgz";
      sha1 = "2d41e88c3e4319797e305f87027ef43f4dc739c4";
    };
    deps = {
      "babel-traverse-6.13.0" = self.by-version."babel-traverse"."6.13.0";
      "babel-helper-call-delegate-6.8.0" = self.by-version."babel-helper-call-delegate"."6.8.0";
      "babel-helper-get-function-arity-6.8.0" = self.by-version."babel-helper-get-function-arity"."6.8.0";
      "babel-template-6.9.0" = self.by-version."babel-template"."6.9.0";
      "babel-types-6.13.0" = self.by-version."babel-types"."6.13.0";
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-plugin-transform-es2015-shorthand-properties"."^6.3.13" =
    self.by-version."babel-plugin-transform-es2015-shorthand-properties"."6.8.0";
  by-version."babel-plugin-transform-es2015-shorthand-properties"."6.8.0" = self.buildNodePackage {
    name = "babel-plugin-transform-es2015-shorthand-properties-6.8.0";
    version = "6.8.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-plugin-transform-es2015-shorthand-properties/-/babel-plugin-transform-es2015-shorthand-properties-6.8.0.tgz";
      name = "babel-plugin-transform-es2015-shorthand-properties-6.8.0.tgz";
      sha1 = "f0a4c5fd471630acf333c2d99c3d677bf0952149";
    };
    deps = {
      "babel-types-6.13.0" = self.by-version."babel-types"."6.13.0";
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-plugin-transform-es2015-spread"."^6.3.13" =
    self.by-version."babel-plugin-transform-es2015-spread"."6.8.0";
  by-version."babel-plugin-transform-es2015-spread"."6.8.0" = self.buildNodePackage {
    name = "babel-plugin-transform-es2015-spread-6.8.0";
    version = "6.8.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-plugin-transform-es2015-spread/-/babel-plugin-transform-es2015-spread-6.8.0.tgz";
      name = "babel-plugin-transform-es2015-spread-6.8.0.tgz";
      sha1 = "0217f737e3b821fa5a669f187c6ed59205f05e9c";
    };
    deps = {
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-plugin-transform-es2015-sticky-regex"."^6.3.13" =
    self.by-version."babel-plugin-transform-es2015-sticky-regex"."6.8.0";
  by-version."babel-plugin-transform-es2015-sticky-regex"."6.8.0" = self.buildNodePackage {
    name = "babel-plugin-transform-es2015-sticky-regex-6.8.0";
    version = "6.8.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-plugin-transform-es2015-sticky-regex/-/babel-plugin-transform-es2015-sticky-regex-6.8.0.tgz";
      name = "babel-plugin-transform-es2015-sticky-regex-6.8.0.tgz";
      sha1 = "e73d300a440a35d5c64f5c2a344dc236e3df47be";
    };
    deps = {
      "babel-helper-regex-6.9.0" = self.by-version."babel-helper-regex"."6.9.0";
      "babel-types-6.13.0" = self.by-version."babel-types"."6.13.0";
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-plugin-transform-es2015-template-literals"."^6.3.13" =
    self.by-version."babel-plugin-transform-es2015-template-literals"."6.8.0";
  by-version."babel-plugin-transform-es2015-template-literals"."6.8.0" = self.buildNodePackage {
    name = "babel-plugin-transform-es2015-template-literals-6.8.0";
    version = "6.8.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-plugin-transform-es2015-template-literals/-/babel-plugin-transform-es2015-template-literals-6.8.0.tgz";
      name = "babel-plugin-transform-es2015-template-literals-6.8.0.tgz";
      sha1 = "86eb876d0a2c635da4ec048b4f7de9dfc897e66b";
    };
    deps = {
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-plugin-transform-es2015-typeof-symbol"."^6.3.13" =
    self.by-version."babel-plugin-transform-es2015-typeof-symbol"."6.8.0";
  by-version."babel-plugin-transform-es2015-typeof-symbol"."6.8.0" = self.buildNodePackage {
    name = "babel-plugin-transform-es2015-typeof-symbol-6.8.0";
    version = "6.8.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-plugin-transform-es2015-typeof-symbol/-/babel-plugin-transform-es2015-typeof-symbol-6.8.0.tgz";
      name = "babel-plugin-transform-es2015-typeof-symbol-6.8.0.tgz";
      sha1 = "84c29eb1219372480955a020fef7a65c44f30533";
    };
    deps = {
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-plugin-transform-es2015-unicode-regex"."^6.3.13" =
    self.by-version."babel-plugin-transform-es2015-unicode-regex"."6.11.0";
  by-version."babel-plugin-transform-es2015-unicode-regex"."6.11.0" = self.buildNodePackage {
    name = "babel-plugin-transform-es2015-unicode-regex-6.11.0";
    version = "6.11.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-plugin-transform-es2015-unicode-regex/-/babel-plugin-transform-es2015-unicode-regex-6.11.0.tgz";
      name = "babel-plugin-transform-es2015-unicode-regex-6.11.0.tgz";
      sha1 = "6298ceabaad88d50a3f4f392d8de997260f6ef2c";
    };
    deps = {
      "babel-helper-regex-6.9.0" = self.by-version."babel-helper-regex"."6.9.0";
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
      "regexpu-core-2.0.0" = self.by-version."regexpu-core"."2.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-plugin-transform-regenerator"."^6.3.13" =
    self.by-version."babel-plugin-transform-regenerator"."6.11.4";
  by-version."babel-plugin-transform-regenerator"."6.11.4" = self.buildNodePackage {
    name = "babel-plugin-transform-regenerator-6.11.4";
    version = "6.11.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-plugin-transform-regenerator/-/babel-plugin-transform-regenerator-6.11.4.tgz";
      name = "babel-plugin-transform-regenerator-6.11.4.tgz";
      sha1 = "266b5d9b33bc766c35dd758d4c4d9153c4bb6abf";
    };
    deps = {
      "babel-plugin-transform-es2015-block-scoping-6.10.1" = self.by-version."babel-plugin-transform-es2015-block-scoping"."6.10.1";
      "babel-plugin-syntax-async-functions-6.13.0" = self.by-version."babel-plugin-syntax-async-functions"."6.13.0";
      "babel-plugin-transform-es2015-for-of-6.8.0" = self.by-version."babel-plugin-transform-es2015-for-of"."6.8.0";
      "babel-core-6.13.2" = self.by-version."babel-core"."6.13.2";
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
      "babel-traverse-6.13.0" = self.by-version."babel-traverse"."6.13.0";
      "babel-types-6.13.0" = self.by-version."babel-types"."6.13.0";
      "babylon-6.9.0" = self.by-version."babylon"."6.9.0";
      "private-0.1.6" = self.by-version."private"."0.1.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-plugin-transform-strict-mode"."^6.8.0" =
    self.by-version."babel-plugin-transform-strict-mode"."6.11.3";
  by-version."babel-plugin-transform-strict-mode"."6.11.3" = self.buildNodePackage {
    name = "babel-plugin-transform-strict-mode-6.11.3";
    version = "6.11.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-plugin-transform-strict-mode/-/babel-plugin-transform-strict-mode-6.11.3.tgz";
      name = "babel-plugin-transform-strict-mode-6.11.3.tgz";
      sha1 = "183741325126bc7ec9cf4c0fc257d3e7ca5afd40";
    };
    deps = {
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
      "babel-types-6.13.0" = self.by-version."babel-types"."6.13.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-preset-es2015"."~6.3.13" =
    self.by-version."babel-preset-es2015"."6.3.13";
  by-version."babel-preset-es2015"."6.3.13" = self.buildNodePackage {
    name = "babel-preset-es2015-6.3.13";
    version = "6.3.13";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-preset-es2015/-/babel-preset-es2015-6.3.13.tgz";
      name = "babel-preset-es2015-6.3.13.tgz";
      sha1 = "97dce7ef292e18cb9b2b7545d80c593c28d9517f";
    };
    deps = {
      "babel-plugin-transform-es2015-template-literals-6.8.0" = self.by-version."babel-plugin-transform-es2015-template-literals"."6.8.0";
      "babel-plugin-transform-es2015-literals-6.8.0" = self.by-version."babel-plugin-transform-es2015-literals"."6.8.0";
      "babel-plugin-transform-es2015-function-name-6.9.0" = self.by-version."babel-plugin-transform-es2015-function-name"."6.9.0";
      "babel-plugin-transform-es2015-arrow-functions-6.8.0" = self.by-version."babel-plugin-transform-es2015-arrow-functions"."6.8.0";
      "babel-plugin-transform-es2015-block-scoped-functions-6.8.0" = self.by-version."babel-plugin-transform-es2015-block-scoped-functions"."6.8.0";
      "babel-plugin-transform-es2015-classes-6.9.0" = self.by-version."babel-plugin-transform-es2015-classes"."6.9.0";
      "babel-plugin-transform-es2015-object-super-6.8.0" = self.by-version."babel-plugin-transform-es2015-object-super"."6.8.0";
      "babel-plugin-transform-es2015-shorthand-properties-6.8.0" = self.by-version."babel-plugin-transform-es2015-shorthand-properties"."6.8.0";
      "babel-plugin-transform-es2015-computed-properties-6.8.0" = self.by-version."babel-plugin-transform-es2015-computed-properties"."6.8.0";
      "babel-plugin-transform-es2015-for-of-6.8.0" = self.by-version."babel-plugin-transform-es2015-for-of"."6.8.0";
      "babel-plugin-transform-es2015-sticky-regex-6.8.0" = self.by-version."babel-plugin-transform-es2015-sticky-regex"."6.8.0";
      "babel-plugin-transform-es2015-unicode-regex-6.11.0" = self.by-version."babel-plugin-transform-es2015-unicode-regex"."6.11.0";
      "babel-plugin-check-es2015-constants-6.8.0" = self.by-version."babel-plugin-check-es2015-constants"."6.8.0";
      "babel-plugin-transform-es2015-spread-6.8.0" = self.by-version."babel-plugin-transform-es2015-spread"."6.8.0";
      "babel-plugin-transform-es2015-parameters-6.11.4" = self.by-version."babel-plugin-transform-es2015-parameters"."6.11.4";
      "babel-plugin-transform-es2015-destructuring-6.9.0" = self.by-version."babel-plugin-transform-es2015-destructuring"."6.9.0";
      "babel-plugin-transform-es2015-block-scoping-6.10.1" = self.by-version."babel-plugin-transform-es2015-block-scoping"."6.10.1";
      "babel-plugin-transform-es2015-typeof-symbol-6.8.0" = self.by-version."babel-plugin-transform-es2015-typeof-symbol"."6.8.0";
      "babel-plugin-transform-es2015-modules-commonjs-6.11.5" = self.by-version."babel-plugin-transform-es2015-modules-commonjs"."6.11.5";
      "babel-plugin-transform-regenerator-6.11.4" = self.by-version."babel-plugin-transform-regenerator"."6.11.4";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "babel-preset-es2015" = self.by-version."babel-preset-es2015"."6.3.13";
  by-spec."babel-register"."^6.3.13" =
    self.by-version."babel-register"."6.11.6";
  by-version."babel-register"."6.11.6" = self.buildNodePackage {
    name = "babel-register-6.11.6";
    version = "6.11.6";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-register/-/babel-register-6.11.6.tgz";
      name = "babel-register-6.11.6.tgz";
      sha1 = "d235f6102b9350fce6384064e0c12d6892680c46";
    };
    deps = {
      "babel-core-6.13.2" = self.by-version."babel-core"."6.13.2";
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
      "core-js-2.4.1" = self.by-version."core-js"."2.4.1";
      "home-or-tmp-1.0.0" = self.by-version."home-or-tmp"."1.0.0";
      "lodash-4.15.0" = self.by-version."lodash"."4.15.0";
      "mkdirp-0.5.1" = self.by-version."mkdirp"."0.5.1";
      "path-exists-1.0.0" = self.by-version."path-exists"."1.0.0";
      "source-map-support-0.2.10" = self.by-version."source-map-support"."0.2.10";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-register"."^6.9.0" =
    self.by-version."babel-register"."6.11.6";
  by-spec."babel-register"."~6.7.2" =
    self.by-version."babel-register"."6.7.2";
  by-version."babel-register"."6.7.2" = self.buildNodePackage {
    name = "babel-register-6.7.2";
    version = "6.7.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-register/-/babel-register-6.7.2.tgz";
      name = "babel-register-6.7.2.tgz";
      sha1 = "4dec809ba2d4ccadd185efb2c0a3f560e1f6c8a0";
    };
    deps = {
      "babel-core-6.13.2" = self.by-version."babel-core"."6.13.2";
      "babel-runtime-5.8.38" = self.by-version."babel-runtime"."5.8.38";
      "core-js-2.4.1" = self.by-version."core-js"."2.4.1";
      "home-or-tmp-1.0.0" = self.by-version."home-or-tmp"."1.0.0";
      "lodash-3.10.1" = self.by-version."lodash"."3.10.1";
      "mkdirp-0.5.1" = self.by-version."mkdirp"."0.5.1";
      "path-exists-1.0.0" = self.by-version."path-exists"."1.0.0";
      "source-map-support-0.2.10" = self.by-version."source-map-support"."0.2.10";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "babel-register" = self.by-version."babel-register"."6.7.2";
  by-spec."babel-runtime"."^5.0.0" =
    self.by-version."babel-runtime"."5.8.38";
  by-version."babel-runtime"."5.8.38" = self.buildNodePackage {
    name = "babel-runtime-5.8.38";
    version = "5.8.38";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-runtime/-/babel-runtime-5.8.38.tgz";
      name = "babel-runtime-5.8.38.tgz";
      sha1 = "1c0b02eb63312f5f087ff20450827b425c9d4c19";
    };
    deps = {
      "core-js-1.2.7" = self.by-version."core-js"."1.2.7";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-runtime"."^6.0.0" =
    self.by-version."babel-runtime"."6.11.6";
  by-version."babel-runtime"."6.11.6" = self.buildNodePackage {
    name = "babel-runtime-6.11.6";
    version = "6.11.6";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-runtime/-/babel-runtime-6.11.6.tgz";
      name = "babel-runtime-6.11.6.tgz";
      sha1 = "6db707fef2d49c49bfa3cb64efdb436b518b8222";
    };
    deps = {
      "core-js-2.4.1" = self.by-version."core-js"."2.4.1";
      "regenerator-runtime-0.9.5" = self.by-version."regenerator-runtime"."0.9.5";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-runtime"."^6.11.6" =
    self.by-version."babel-runtime"."6.11.6";
  by-spec."babel-runtime"."^6.9.0" =
    self.by-version."babel-runtime"."6.11.6";
  by-spec."babel-runtime"."^6.9.1" =
    self.by-version."babel-runtime"."6.11.6";
  by-spec."babel-template"."^6.3.13" =
    self.by-version."babel-template"."6.9.0";
  by-version."babel-template"."6.9.0" = self.buildNodePackage {
    name = "babel-template-6.9.0";
    version = "6.9.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-template/-/babel-template-6.9.0.tgz";
      name = "babel-template-6.9.0.tgz";
      sha1 = "97090fcf6bc15685b4f05be65c0a9438aa7e23e3";
    };
    deps = {
      "babylon-6.9.0" = self.by-version."babylon"."6.9.0";
      "babel-traverse-6.13.0" = self.by-version."babel-traverse"."6.13.0";
      "babel-types-6.13.0" = self.by-version."babel-types"."6.13.0";
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
      "lodash-4.15.0" = self.by-version."lodash"."4.15.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-template"."^6.8.0" =
    self.by-version."babel-template"."6.9.0";
  by-spec."babel-template"."^6.9.0" =
    self.by-version."babel-template"."6.9.0";
  by-spec."babel-traverse"."^6.11.4" =
    self.by-version."babel-traverse"."6.13.0";
  by-version."babel-traverse"."6.13.0" = self.buildNodePackage {
    name = "babel-traverse-6.13.0";
    version = "6.13.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-traverse/-/babel-traverse-6.13.0.tgz";
      name = "babel-traverse-6.13.0.tgz";
      sha1 = "8835b4abae31814e8f7adebb8296b8c7ad0cecc1";
    };
    deps = {
      "babel-code-frame-6.11.0" = self.by-version."babel-code-frame"."6.11.0";
      "babel-messages-6.8.0" = self.by-version."babel-messages"."6.8.0";
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
      "babel-types-6.13.0" = self.by-version."babel-types"."6.13.0";
      "babylon-6.9.0" = self.by-version."babylon"."6.9.0";
      "debug-2.2.0" = self.by-version."debug"."2.2.0";
      "globals-8.18.0" = self.by-version."globals"."8.18.0";
      "invariant-2.2.1" = self.by-version."invariant"."2.2.1";
      "lodash-4.15.0" = self.by-version."lodash"."4.15.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-traverse"."^6.13.0" =
    self.by-version."babel-traverse"."6.13.0";
  by-spec."babel-traverse"."^6.3.26" =
    self.by-version."babel-traverse"."6.13.0";
  by-spec."babel-traverse"."^6.8.0" =
    self.by-version."babel-traverse"."6.13.0";
  by-spec."babel-traverse"."^6.9.0" =
    self.by-version."babel-traverse"."6.13.0";
  by-spec."babel-types"."^6.10.0" =
    self.by-version."babel-types"."6.13.0";
  by-version."babel-types"."6.13.0" = self.buildNodePackage {
    name = "babel-types-6.13.0";
    version = "6.13.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/babel-types/-/babel-types-6.13.0.tgz";
      name = "babel-types-6.13.0.tgz";
      sha1 = "f0809fd635e33304691b437379bffad39d58792b";
    };
    deps = {
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
      "babel-traverse-6.13.0" = self.by-version."babel-traverse"."6.13.0";
      "esutils-2.0.2" = self.by-version."esutils"."2.0.2";
      "lodash-4.15.0" = self.by-version."lodash"."4.15.0";
      "to-fast-properties-1.0.2" = self.by-version."to-fast-properties"."1.0.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babel-types"."^6.10.2" =
    self.by-version."babel-types"."6.13.0";
  by-spec."babel-types"."^6.13.0" =
    self.by-version."babel-types"."6.13.0";
  by-spec."babel-types"."^6.3.21" =
    self.by-version."babel-types"."6.13.0";
  by-spec."babel-types"."^6.8.0" =
    self.by-version."babel-types"."6.13.0";
  by-spec."babel-types"."^6.9.0" =
    self.by-version."babel-types"."6.13.0";
  by-spec."babylon"."^6.3.26" =
    self.by-version."babylon"."6.9.0";
  by-version."babylon"."6.9.0" = self.buildNodePackage {
    name = "babylon-6.9.0";
    version = "6.9.0";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/babylon/-/babylon-6.9.0.tgz";
      name = "babylon-6.9.0.tgz";
      sha1 = "d840d52e44cbb168c11a98bf0282293ab27f7d53";
    };
    deps = {
      "babel-runtime-6.11.6" = self.by-version."babel-runtime"."6.11.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."babylon"."^6.6.5" =
    self.by-version."babylon"."6.9.0";
  by-spec."babylon"."^6.7.0" =
    self.by-version."babylon"."6.9.0";
  by-spec."backo2"."1.0.2" =
    self.by-version."backo2"."1.0.2";
  by-version."backo2"."1.0.2" = self.buildNodePackage {
    name = "backo2-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/backo2/-/backo2-1.0.2.tgz";
      name = "backo2-1.0.2.tgz";
      sha1 = "31ab1ac8b129363463e35b3ebb69f4dfcfba7947";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."balanced-match"."^0.4.1" =
    self.by-version."balanced-match"."0.4.2";
  by-version."balanced-match"."0.4.2" = self.buildNodePackage {
    name = "balanced-match-0.4.2";
    version = "0.4.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/balanced-match/-/balanced-match-0.4.2.tgz";
      name = "balanced-match-0.4.2.tgz";
      sha1 = "cb3f3e3c732dc0f01ee70b403f302e61d7709838";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."base64-arraybuffer"."0.1.2" =
    self.by-version."base64-arraybuffer"."0.1.2";
  by-version."base64-arraybuffer"."0.1.2" = self.buildNodePackage {
    name = "base64-arraybuffer-0.1.2";
    version = "0.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/base64-arraybuffer/-/base64-arraybuffer-0.1.2.tgz";
      name = "base64-arraybuffer-0.1.2.tgz";
      sha1 = "474df4a9f2da24e05df3158c3b1db3c3cd46a154";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."base64id"."0.1.0" =
    self.by-version."base64id"."0.1.0";
  by-version."base64id"."0.1.0" = self.buildNodePackage {
    name = "base64id-0.1.0";
    version = "0.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/base64id/-/base64id-0.1.0.tgz";
      name = "base64id-0.1.0.tgz";
      sha1 = "02ce0fdeee0cef4f40080e1e73e834f0b1bfce3f";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."batch"."0.5.3" =
    self.by-version."batch"."0.5.3";
  by-version."batch"."0.5.3" = self.buildNodePackage {
    name = "batch-0.5.3";
    version = "0.5.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/batch/-/batch-0.5.3.tgz";
      name = "batch-0.5.3.tgz";
      sha1 = "3f3414f380321743bfc1042f9a83ff1d5824d464";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."batch"."^0.5.2" =
    self.by-version."batch"."0.5.3";
  by-spec."beepbeep"."~1.2.0" =
    self.by-version."beepbeep"."1.2.0";
  by-version."beepbeep"."1.2.0" = self.buildNodePackage {
    name = "beepbeep-1.2.0";
    version = "1.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/beepbeep/-/beepbeep-1.2.0.tgz";
      name = "beepbeep-1.2.0.tgz";
      sha1 = "4ce0a48d1492ca24e7cc4421168147b120a672ad";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "beepbeep" = self.by-version."beepbeep"."1.2.0";
  by-spec."beeper"."^1.0.0" =
    self.by-version."beeper"."1.1.0";
  by-version."beeper"."1.1.0" = self.buildNodePackage {
    name = "beeper-1.1.0";
    version = "1.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/beeper/-/beeper-1.1.0.tgz";
      name = "beeper-1.1.0.tgz";
      sha1 = "9ee6fc1ce7f54feaace7ce73588b056037866a2c";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."benchmark"."1.0.0" =
    self.by-version."benchmark"."1.0.0";
  by-version."benchmark"."1.0.0" = self.buildNodePackage {
    name = "benchmark-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/benchmark/-/benchmark-1.0.0.tgz";
      name = "benchmark-1.0.0.tgz";
      sha1 = "2f1e2fa4c359f11122aa183082218e957e390c73";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."better-assert"."~1.0.0" =
    self.by-version."better-assert"."1.0.2";
  by-version."better-assert"."1.0.2" = self.buildNodePackage {
    name = "better-assert-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/better-assert/-/better-assert-1.0.2.tgz";
      name = "better-assert-1.0.2.tgz";
      sha1 = "40866b9e1b9e0b55b481894311e68faffaebc522";
    };
    deps = {
      "callsite-1.0.0" = self.by-version."callsite"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."bin-build"."^2.0.0" =
    self.by-version."bin-build"."2.2.0";
  by-version."bin-build"."2.2.0" = self.buildNodePackage {
    name = "bin-build-2.2.0";
    version = "2.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/bin-build/-/bin-build-2.2.0.tgz";
      name = "bin-build-2.2.0.tgz";
      sha1 = "11f8dd61f70ffcfa2bdcaa5b46f5e8fedd4221cc";
    };
    deps = {
      "archive-type-3.2.0" = self.by-version."archive-type"."3.2.0";
      "decompress-3.0.0" = self.by-version."decompress"."3.0.0";
      "download-4.4.3" = self.by-version."download"."4.4.3";
      "exec-series-1.0.3" = self.by-version."exec-series"."1.0.3";
      "rimraf-2.5.4" = self.by-version."rimraf"."2.5.4";
      "tempfile-1.1.1" = self.by-version."tempfile"."1.1.1";
      "url-regex-3.2.0" = self.by-version."url-regex"."3.2.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."bin-check"."^2.0.0" =
    self.by-version."bin-check"."2.0.0";
  by-version."bin-check"."2.0.0" = self.buildNodePackage {
    name = "bin-check-2.0.0";
    version = "2.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/bin-check/-/bin-check-2.0.0.tgz";
      name = "bin-check-2.0.0.tgz";
      sha1 = "86f8e6f4253893df60dc316957f5af02acb05930";
    };
    deps = {
      "executable-1.1.0" = self.by-version."executable"."1.1.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."bin-version"."^1.0.0" =
    self.by-version."bin-version"."1.0.4";
  by-version."bin-version"."1.0.4" = self.buildNodePackage {
    name = "bin-version-1.0.4";
    version = "1.0.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/bin-version/-/bin-version-1.0.4.tgz";
      name = "bin-version-1.0.4.tgz";
      sha1 = "9eb498ee6fd76f7ab9a7c160436f89579435d78e";
    };
    deps = {
      "find-versions-1.2.1" = self.by-version."find-versions"."1.2.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."bin-version-check"."^2.1.0" =
    self.by-version."bin-version-check"."2.1.0";
  by-version."bin-version-check"."2.1.0" = self.buildNodePackage {
    name = "bin-version-check-2.1.0";
    version = "2.1.0";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/bin-version-check/-/bin-version-check-2.1.0.tgz";
      name = "bin-version-check-2.1.0.tgz";
      sha1 = "e4e5df290b9069f7d111324031efc13fdd11a5b0";
    };
    deps = {
      "bin-version-1.0.4" = self.by-version."bin-version"."1.0.4";
      "minimist-1.2.0" = self.by-version."minimist"."1.2.0";
      "semver-4.3.6" = self.by-version."semver"."4.3.6";
      "semver-truncate-1.1.2" = self.by-version."semver-truncate"."1.1.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."bin-wrapper"."^3.0.0" =
    self.by-version."bin-wrapper"."3.0.2";
  by-version."bin-wrapper"."3.0.2" = self.buildNodePackage {
    name = "bin-wrapper-3.0.2";
    version = "3.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/bin-wrapper/-/bin-wrapper-3.0.2.tgz";
      name = "bin-wrapper-3.0.2.tgz";
      sha1 = "67d3306262e4b1a5f2f88ee23464f6a655677aeb";
    };
    deps = {
      "bin-check-2.0.0" = self.by-version."bin-check"."2.0.0";
      "bin-version-check-2.1.0" = self.by-version."bin-version-check"."2.1.0";
      "download-4.4.3" = self.by-version."download"."4.4.3";
      "each-async-1.1.1" = self.by-version."each-async"."1.1.1";
      "lazy-req-1.1.0" = self.by-version."lazy-req"."1.1.0";
      "os-filter-obj-1.0.3" = self.by-version."os-filter-obj"."1.0.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."binary-extensions"."^1.0.0" =
    self.by-version."binary-extensions"."1.5.0";
  by-version."binary-extensions"."1.5.0" = self.buildNodePackage {
    name = "binary-extensions-1.5.0";
    version = "1.5.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/binary-extensions/-/binary-extensions-1.5.0.tgz";
      name = "binary-extensions-1.5.0.tgz";
      sha1 = "e6e2057f2cdfb17ad406349c86b71ef8069a25f5";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."binaryextensions"."~1.0.0" =
    self.by-version."binaryextensions"."1.0.1";
  by-version."binaryextensions"."1.0.1" = self.buildNodePackage {
    name = "binaryextensions-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/binaryextensions/-/binaryextensions-1.0.1.tgz";
      name = "binaryextensions-1.0.1.tgz";
      sha1 = "1e637488b35b58bda5f4774bf96a5212a8c90755";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."bl"."^1.0.0" =
    self.by-version."bl"."1.1.2";
  by-version."bl"."1.1.2" = self.buildNodePackage {
    name = "bl-1.1.2";
    version = "1.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/bl/-/bl-1.1.2.tgz";
      name = "bl-1.1.2.tgz";
      sha1 = "fdca871a99713aa00d19e3bbba41c44787a65398";
    };
    deps = {
      "readable-stream-2.0.6" = self.by-version."readable-stream"."2.0.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."bl"."~1.0.0" =
    self.by-version."bl"."1.0.3";
  by-version."bl"."1.0.3" = self.buildNodePackage {
    name = "bl-1.0.3";
    version = "1.0.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/bl/-/bl-1.0.3.tgz";
      name = "bl-1.0.3.tgz";
      sha1 = "fc5421a28fd4226036c3b3891a66a25bc64d226e";
    };
    deps = {
      "readable-stream-2.0.6" = self.by-version."readable-stream"."2.0.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."bl"."~1.1.2" =
    self.by-version."bl"."1.1.2";
  by-spec."blob"."0.0.4" =
    self.by-version."blob"."0.0.4";
  by-version."blob"."0.0.4" = self.buildNodePackage {
    name = "blob-0.0.4";
    version = "0.0.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/blob/-/blob-0.0.4.tgz";
      name = "blob-0.0.4.tgz";
      sha1 = "bcf13052ca54463f30f9fc7e95b9a47630a94921";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."block-stream"."*" =
    self.by-version."block-stream"."0.0.9";
  by-version."block-stream"."0.0.9" = self.buildNodePackage {
    name = "block-stream-0.0.9";
    version = "0.0.9";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/block-stream/-/block-stream-0.0.9.tgz";
      name = "block-stream-0.0.9.tgz";
      sha1 = "13ebfe778a03205cfe03751481ebb4b3300c126a";
    };
    deps = {
      "inherits-2.0.1" = self.by-version."inherits"."2.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."bluebird"."^2.9.25" =
    self.by-version."bluebird"."2.10.2";
  by-version."bluebird"."2.10.2" = self.buildNodePackage {
    name = "bluebird-2.10.2";
    version = "2.10.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/bluebird/-/bluebird-2.10.2.tgz";
      name = "bluebird-2.10.2.tgz";
      sha1 = "024a5517295308857f14f91f1106fc3b555f446b";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."bluebird"."~1.2.4" =
    self.by-version."bluebird"."1.2.4";
  by-version."bluebird"."1.2.4" = self.buildNodePackage {
    name = "bluebird-1.2.4";
    version = "1.2.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/bluebird/-/bluebird-1.2.4.tgz";
      name = "bluebird-1.2.4.tgz";
      sha1 = "5985ec23cb6ff1a5834cc6447b3c5ef010fd321a";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."boolbase"."~1.0.0" =
    self.by-version."boolbase"."1.0.0";
  by-version."boolbase"."1.0.0" = self.buildNodePackage {
    name = "boolbase-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/boolbase/-/boolbase-1.0.0.tgz";
      name = "boolbase-1.0.0.tgz";
      sha1 = "68dff5fbe60c51eb37725ea9e3ed310dcc1e776e";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."boom"."0.4.x" =
    self.by-version."boom"."0.4.2";
  by-version."boom"."0.4.2" = self.buildNodePackage {
    name = "boom-0.4.2";
    version = "0.4.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/boom/-/boom-0.4.2.tgz";
      name = "boom-0.4.2.tgz";
      sha1 = "7a636e9ded4efcefb19cef4947a3c67dfaee911b";
    };
    deps = {
      "hoek-0.9.1" = self.by-version."hoek"."0.9.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."boom"."2.x.x" =
    self.by-version."boom"."2.10.1";
  by-version."boom"."2.10.1" = self.buildNodePackage {
    name = "boom-2.10.1";
    version = "2.10.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/boom/-/boom-2.10.1.tgz";
      name = "boom-2.10.1.tgz";
      sha1 = "39c8918ceff5799f83f9492a848f625add0c766f";
    };
    deps = {
      "hoek-2.16.3" = self.by-version."hoek"."2.16.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."brace-expansion"."^1.0.0" =
    self.by-version."brace-expansion"."1.1.6";
  by-version."brace-expansion"."1.1.6" = self.buildNodePackage {
    name = "brace-expansion-1.1.6";
    version = "1.1.6";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/brace-expansion/-/brace-expansion-1.1.6.tgz";
      name = "brace-expansion-1.1.6.tgz";
      sha1 = "7197d7eaa9b87e648390ea61fc66c84427420df9";
    };
    deps = {
      "balanced-match-0.4.2" = self.by-version."balanced-match"."0.4.2";
      "concat-map-0.0.1" = self.by-version."concat-map"."0.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."braces"."^1.8.1" =
    self.by-version."braces"."1.8.5";
  by-version."braces"."1.8.5" = self.buildNodePackage {
    name = "braces-1.8.5";
    version = "1.8.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/braces/-/braces-1.8.5.tgz";
      name = "braces-1.8.5.tgz";
      sha1 = "ba77962e12dff969d6b76711e914b737857bf6a7";
    };
    deps = {
      "expand-range-1.8.2" = self.by-version."expand-range"."1.8.2";
      "preserve-0.2.0" = self.by-version."preserve"."0.2.0";
      "repeat-element-1.1.2" = self.by-version."repeat-element"."1.1.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."braces"."^1.8.2" =
    self.by-version."braces"."1.8.5";
  by-spec."browser-sync"."~2.11.0" =
    self.by-version."browser-sync"."2.11.2";
  by-version."browser-sync"."2.11.2" = self.buildNodePackage {
    name = "browser-sync-2.11.2";
    version = "2.11.2";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/browser-sync/-/browser-sync-2.11.2.tgz";
      name = "browser-sync-2.11.2.tgz";
      sha1 = "be3aab58349da70a1ab0314e765f396d684c39d5";
    };
    deps = {
      "async-each-series-0.1.1" = self.by-version."async-each-series"."0.1.1";
      "browser-sync-client-2.4.2" = self.by-version."browser-sync-client"."2.4.2";
      "browser-sync-ui-0.5.19" = self.by-version."browser-sync-ui"."0.5.19";
      "bs-recipes-1.2.3" = self.by-version."bs-recipes"."1.2.3";
      "chokidar-1.4.1" = self.by-version."chokidar"."1.4.1";
      "connect-3.4.1" = self.by-version."connect"."3.4.1";
      "dev-ip-1.0.1" = self.by-version."dev-ip"."1.0.1";
      "easy-extender-2.3.2" = self.by-version."easy-extender"."2.3.2";
      "eazy-logger-2.1.3" = self.by-version."eazy-logger"."2.1.3";
      "emitter-steward-1.0.0" = self.by-version."emitter-steward"."1.0.0";
      "foxy-11.1.5" = self.by-version."foxy"."11.1.5";
      "fs-extra-0.26.7" = self.by-version."fs-extra"."0.26.7";
      "immutable-3.8.1" = self.by-version."immutable"."3.8.1";
      "localtunnel-1.8.1" = self.by-version."localtunnel"."1.8.1";
      "lodash-3.10.1" = self.by-version."lodash"."3.10.1";
      "longest-1.0.1" = self.by-version."longest"."1.0.1";
      "meow-3.3.0" = self.by-version."meow"."3.3.0";
      "micromatch-2.3.5" = self.by-version."micromatch"."2.3.5";
      "opn-3.0.3" = self.by-version."opn"."3.0.3";
      "portscanner-1.0.0" = self.by-version."portscanner"."1.0.0";
      "query-string-2.4.2" = self.by-version."query-string"."2.4.2";
      "resp-modifier-5.0.2" = self.by-version."resp-modifier"."5.0.2";
      "serve-index-1.8.0" = self.by-version."serve-index"."1.8.0";
      "serve-static-1.11.1" = self.by-version."serve-static"."1.11.1";
      "socket.io-1.4.5" = self.by-version."socket.io"."1.4.5";
      "ua-parser-js-0.7.10" = self.by-version."ua-parser-js"."0.7.10";
      "ucfirst-1.0.0" = self.by-version."ucfirst"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "browser-sync" = self.by-version."browser-sync"."2.11.2";
  by-spec."browser-sync-client"."^2.3.3" =
    self.by-version."browser-sync-client"."2.4.2";
  by-version."browser-sync-client"."2.4.2" = self.buildNodePackage {
    name = "browser-sync-client-2.4.2";
    version = "2.4.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/browser-sync-client/-/browser-sync-client-2.4.2.tgz";
      name = "browser-sync-client-2.4.2.tgz";
      sha1 = "0c510d4183c0b5dbc4044363facc3a4273f7ba28";
    };
    deps = {
      "etag-1.7.0" = self.by-version."etag"."1.7.0";
      "fresh-0.3.0" = self.by-version."fresh"."0.3.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."browser-sync-ui"."^0.5.16" =
    self.by-version."browser-sync-ui"."0.5.19";
  by-version."browser-sync-ui"."0.5.19" = self.buildNodePackage {
    name = "browser-sync-ui-0.5.19";
    version = "0.5.19";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/browser-sync-ui/-/browser-sync-ui-0.5.19.tgz";
      name = "browser-sync-ui-0.5.19.tgz";
      sha1 = "1003ff6bc52d091f0f724054263721fb6a2dbf7b";
    };
    deps = {
      "async-each-series-0.1.1" = self.by-version."async-each-series"."0.1.1";
      "connect-history-api-fallback-1.3.0" = self.by-version."connect-history-api-fallback"."1.3.0";
      "immutable-3.8.1" = self.by-version."immutable"."3.8.1";
      "stream-throttle-0.1.3" = self.by-version."stream-throttle"."0.1.3";
      "weinre-2.0.0-pre-I0Z7U9OV" = self.by-version."weinre"."2.0.0-pre-I0Z7U9OV";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."bs-recipes"."^1.0.5" =
    self.by-version."bs-recipes"."1.2.3";
  by-version."bs-recipes"."1.2.3" = self.buildNodePackage {
    name = "bs-recipes-1.2.3";
    version = "1.2.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/bs-recipes/-/bs-recipes-1.2.3.tgz";
      name = "bs-recipes-1.2.3.tgz";
      sha1 = "0e4d17bb1cff92ef6c36608b8487d9a07571ac54";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."buffer-crc32"."~0.2.3" =
    self.by-version."buffer-crc32"."0.2.5";
  by-version."buffer-crc32"."0.2.5" = self.buildNodePackage {
    name = "buffer-crc32-0.2.5";
    version = "0.2.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/buffer-crc32/-/buffer-crc32-0.2.5.tgz";
      name = "buffer-crc32-0.2.5.tgz";
      sha1 = "db003ac2671e62ebd6ece78ea2c2e1b405736e91";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."buffer-shims"."^1.0.0" =
    self.by-version."buffer-shims"."1.0.0";
  by-version."buffer-shims"."1.0.0" = self.buildNodePackage {
    name = "buffer-shims-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/buffer-shims/-/buffer-shims-1.0.0.tgz";
      name = "buffer-shims-1.0.0.tgz";
      sha1 = "9978ce317388c649ad8793028c3477ef044a8b51";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."buffer-to-vinyl"."^1.0.0" =
    self.by-version."buffer-to-vinyl"."1.1.0";
  by-version."buffer-to-vinyl"."1.1.0" = self.buildNodePackage {
    name = "buffer-to-vinyl-1.1.0";
    version = "1.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/buffer-to-vinyl/-/buffer-to-vinyl-1.1.0.tgz";
      name = "buffer-to-vinyl-1.1.0.tgz";
      sha1 = "00f15faee3ab7a1dda2cde6d9121bffdd07b2262";
    };
    deps = {
      "file-type-3.8.0" = self.by-version."file-type"."3.8.0";
      "readable-stream-2.1.5" = self.by-version."readable-stream"."2.1.5";
      "uuid-2.0.2" = self.by-version."uuid"."2.0.2";
      "vinyl-1.2.0" = self.by-version."vinyl"."1.2.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."bufferstreams"."^1.1.0" =
    self.by-version."bufferstreams"."1.1.1";
  by-version."bufferstreams"."1.1.1" = self.buildNodePackage {
    name = "bufferstreams-1.1.1";
    version = "1.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/bufferstreams/-/bufferstreams-1.1.1.tgz";
      name = "bufferstreams-1.1.1.tgz";
      sha1 = "0161373060ac5988eff99058731114f6e195d51e";
    };
    deps = {
      "readable-stream-2.1.5" = self.by-version."readable-stream"."2.1.5";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."builtin-modules"."^1.0.0" =
    self.by-version."builtin-modules"."1.1.1";
  by-version."builtin-modules"."1.1.1" = self.buildNodePackage {
    name = "builtin-modules-1.1.1";
    version = "1.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/builtin-modules/-/builtin-modules-1.1.1.tgz";
      name = "builtin-modules-1.1.1.tgz";
      sha1 = "270f076c5a72c02f5b65a47df94c5fe3a278892f";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."callsite"."1.0.0" =
    self.by-version."callsite"."1.0.0";
  by-version."callsite"."1.0.0" = self.buildNodePackage {
    name = "callsite-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/callsite/-/callsite-1.0.0.tgz";
      name = "callsite-1.0.0.tgz";
      sha1 = "280398e5d664bd74038b6f0905153e6e8af1bc20";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."camel-case"."^1.1.1" =
    self.by-version."camel-case"."1.2.2";
  by-version."camel-case"."1.2.2" = self.buildNodePackage {
    name = "camel-case-1.2.2";
    version = "1.2.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/camel-case/-/camel-case-1.2.2.tgz";
      name = "camel-case-1.2.2.tgz";
      sha1 = "1aca7c4d195359a2ce9955793433c6e5542511f2";
    };
    deps = {
      "sentence-case-1.1.3" = self.by-version."sentence-case"."1.1.3";
      "upper-case-1.1.3" = self.by-version."upper-case"."1.1.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."camelcase"."^1.0.1" =
    self.by-version."camelcase"."1.2.1";
  by-version."camelcase"."1.2.1" = self.buildNodePackage {
    name = "camelcase-1.2.1";
    version = "1.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/camelcase/-/camelcase-1.2.1.tgz";
      name = "camelcase-1.2.1.tgz";
      sha1 = "9bb5304d2e0b56698b2c758b08a3eaa9daa58a39";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."camelcase"."^1.0.2" =
    self.by-version."camelcase"."1.2.1";
  by-spec."camelcase"."^1.2.1" =
    self.by-version."camelcase"."1.2.1";
  by-spec."camelcase"."^2.0.0" =
    self.by-version."camelcase"."2.1.1";
  by-version."camelcase"."2.1.1" = self.buildNodePackage {
    name = "camelcase-2.1.1";
    version = "2.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/camelcase/-/camelcase-2.1.1.tgz";
      name = "camelcase-2.1.1.tgz";
      sha1 = "7c1d16d679a1bbe59ca02cacecfb011e201f5a1f";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."camelcase"."^2.0.1" =
    self.by-version."camelcase"."2.1.1";
  by-spec."camelcase"."^3.0.0" =
    self.by-version."camelcase"."3.0.0";
  by-version."camelcase"."3.0.0" = self.buildNodePackage {
    name = "camelcase-3.0.0";
    version = "3.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/camelcase/-/camelcase-3.0.0.tgz";
      name = "camelcase-3.0.0.tgz";
      sha1 = "32fc4b9fcdaf845fcdf7e73bb97cac2261f0ab0a";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."camelcase-keys"."^1.0.0" =
    self.by-version."camelcase-keys"."1.0.0";
  by-version."camelcase-keys"."1.0.0" = self.buildNodePackage {
    name = "camelcase-keys-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/camelcase-keys/-/camelcase-keys-1.0.0.tgz";
      name = "camelcase-keys-1.0.0.tgz";
      sha1 = "bd1a11bf9b31a1ce493493a930de1a0baf4ad7ec";
    };
    deps = {
      "camelcase-1.2.1" = self.by-version."camelcase"."1.2.1";
      "map-obj-1.0.1" = self.by-version."map-obj"."1.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."camelcase-keys"."^2.0.0" =
    self.by-version."camelcase-keys"."2.1.0";
  by-version."camelcase-keys"."2.1.0" = self.buildNodePackage {
    name = "camelcase-keys-2.1.0";
    version = "2.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/camelcase-keys/-/camelcase-keys-2.1.0.tgz";
      name = "camelcase-keys-2.1.0.tgz";
      sha1 = "308beeaffdf28119051efa1d932213c91b8f92e7";
    };
    deps = {
      "camelcase-2.1.1" = self.by-version."camelcase"."2.1.1";
      "map-obj-1.0.1" = self.by-version."map-obj"."1.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."capture-stack-trace"."^1.0.0" =
    self.by-version."capture-stack-trace"."1.0.0";
  by-version."capture-stack-trace"."1.0.0" = self.buildNodePackage {
    name = "capture-stack-trace-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/capture-stack-trace/-/capture-stack-trace-1.0.0.tgz";
      name = "capture-stack-trace-1.0.0.tgz";
      sha1 = "4a6fa07399c26bba47f0b2496b4d0fb408c5550d";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."caseless"."~0.11.0" =
    self.by-version."caseless"."0.11.0";
  by-version."caseless"."0.11.0" = self.buildNodePackage {
    name = "caseless-0.11.0";
    version = "0.11.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/caseless/-/caseless-0.11.0.tgz";
      name = "caseless-0.11.0.tgz";
      sha1 = "715b96ea9841593cc33067923f5ec60ebda4f7d7";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."caw"."^1.0.1" =
    self.by-version."caw"."1.2.0";
  by-version."caw"."1.2.0" = self.buildNodePackage {
    name = "caw-1.2.0";
    version = "1.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/caw/-/caw-1.2.0.tgz";
      name = "caw-1.2.0.tgz";
      sha1 = "ffb226fe7efc547288dc62ee3e97073c212d1034";
    };
    deps = {
      "get-proxy-1.1.0" = self.by-version."get-proxy"."1.1.0";
      "is-obj-1.0.1" = self.by-version."is-obj"."1.0.1";
      "object-assign-3.0.0" = self.by-version."object-assign"."3.0.0";
      "tunnel-agent-0.4.3" = self.by-version."tunnel-agent"."0.4.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."center-align"."^0.1.1" =
    self.by-version."center-align"."0.1.3";
  by-version."center-align"."0.1.3" = self.buildNodePackage {
    name = "center-align-0.1.3";
    version = "0.1.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/center-align/-/center-align-0.1.3.tgz";
      name = "center-align-0.1.3.tgz";
      sha1 = "aa0d32629b6ee972200411cbd4461c907bc2b7ad";
    };
    deps = {
      "align-text-0.1.4" = self.by-version."align-text"."0.1.4";
      "lazy-cache-1.0.4" = self.by-version."lazy-cache"."1.0.4";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."chalk"."*" =
    self.by-version."chalk"."1.1.3";
  by-version."chalk"."1.1.3" = self.buildNodePackage {
    name = "chalk-1.1.3";
    version = "1.1.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/chalk/-/chalk-1.1.3.tgz";
      name = "chalk-1.1.3.tgz";
      sha1 = "a8115c55e4a702fe4d150abd3872822a7e09fc98";
    };
    deps = {
      "ansi-styles-2.2.1" = self.by-version."ansi-styles"."2.2.1";
      "escape-string-regexp-1.0.5" = self.by-version."escape-string-regexp"."1.0.5";
      "has-ansi-2.0.0" = self.by-version."has-ansi"."2.0.0";
      "strip-ansi-3.0.1" = self.by-version."strip-ansi"."3.0.1";
      "supports-color-2.0.0" = self.by-version."supports-color"."2.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."chalk"."^0.5.0" =
    self.by-version."chalk"."0.5.1";
  by-version."chalk"."0.5.1" = self.buildNodePackage {
    name = "chalk-0.5.1";
    version = "0.5.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/chalk/-/chalk-0.5.1.tgz";
      name = "chalk-0.5.1.tgz";
      sha1 = "663b3a648b68b55d04690d49167aa837858f2174";
    };
    deps = {
      "ansi-styles-1.1.0" = self.by-version."ansi-styles"."1.1.0";
      "escape-string-regexp-1.0.5" = self.by-version."escape-string-regexp"."1.0.5";
      "has-ansi-0.1.0" = self.by-version."has-ansi"."0.1.0";
      "strip-ansi-0.3.0" = self.by-version."strip-ansi"."0.3.0";
      "supports-color-0.2.0" = self.by-version."supports-color"."0.2.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."chalk"."^1.0.0" =
    self.by-version."chalk"."1.1.3";
  by-spec."chalk"."^1.1.0" =
    self.by-version."chalk"."1.1.3";
  by-spec."chalk"."^1.1.1" =
    self.by-version."chalk"."1.1.3";
  by-spec."chalk"."^1.1.3" =
    self.by-version."chalk"."1.1.3";
  by-spec."chalk"."~0.4.0" =
    self.by-version."chalk"."0.4.0";
  by-version."chalk"."0.4.0" = self.buildNodePackage {
    name = "chalk-0.4.0";
    version = "0.4.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/chalk/-/chalk-0.4.0.tgz";
      name = "chalk-0.4.0.tgz";
      sha1 = "5199a3ddcd0c1efe23bc08c1b027b06176e0c64f";
    };
    deps = {
      "has-color-0.1.7" = self.by-version."has-color"."0.1.7";
      "ansi-styles-1.0.0" = self.by-version."ansi-styles"."1.0.0";
      "strip-ansi-0.1.1" = self.by-version."strip-ansi"."0.1.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."change-case"."2.3.x" =
    self.by-version."change-case"."2.3.1";
  by-version."change-case"."2.3.1" = self.buildNodePackage {
    name = "change-case-2.3.1";
    version = "2.3.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/change-case/-/change-case-2.3.1.tgz";
      name = "change-case-2.3.1.tgz";
      sha1 = "2c4fde3f063bb41d00cd68e0d5a09db61cbe894f";
    };
    deps = {
      "camel-case-1.2.2" = self.by-version."camel-case"."1.2.2";
      "constant-case-1.1.2" = self.by-version."constant-case"."1.1.2";
      "dot-case-1.1.2" = self.by-version."dot-case"."1.1.2";
      "is-lower-case-1.1.3" = self.by-version."is-lower-case"."1.1.3";
      "is-upper-case-1.1.2" = self.by-version."is-upper-case"."1.1.2";
      "lower-case-1.1.3" = self.by-version."lower-case"."1.1.3";
      "lower-case-first-1.0.2" = self.by-version."lower-case-first"."1.0.2";
      "param-case-1.1.2" = self.by-version."param-case"."1.1.2";
      "pascal-case-1.1.2" = self.by-version."pascal-case"."1.1.2";
      "path-case-1.1.2" = self.by-version."path-case"."1.1.2";
      "sentence-case-1.1.3" = self.by-version."sentence-case"."1.1.3";
      "snake-case-1.1.2" = self.by-version."snake-case"."1.1.2";
      "swap-case-1.1.2" = self.by-version."swap-case"."1.1.2";
      "title-case-1.1.2" = self.by-version."title-case"."1.1.2";
      "upper-case-1.1.3" = self.by-version."upper-case"."1.1.3";
      "upper-case-first-1.1.2" = self.by-version."upper-case-first"."1.1.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."cheerio"."^0.19.0" =
    self.by-version."cheerio"."0.19.0";
  by-version."cheerio"."0.19.0" = self.buildNodePackage {
    name = "cheerio-0.19.0";
    version = "0.19.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/cheerio/-/cheerio-0.19.0.tgz";
      name = "cheerio-0.19.0.tgz";
      sha1 = "772e7015f2ee29965096d71ea4175b75ab354925";
    };
    deps = {
      "css-select-1.0.0" = self.by-version."css-select"."1.0.0";
      "entities-1.1.1" = self.by-version."entities"."1.1.1";
      "htmlparser2-3.8.3" = self.by-version."htmlparser2"."3.8.3";
      "dom-serializer-0.1.0" = self.by-version."dom-serializer"."0.1.0";
      "lodash-3.10.1" = self.by-version."lodash"."3.10.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."cheerio"."^0.20.0" =
    self.by-version."cheerio"."0.20.0";
  by-version."cheerio"."0.20.0" = self.buildNodePackage {
    name = "cheerio-0.20.0";
    version = "0.20.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/cheerio/-/cheerio-0.20.0.tgz";
      name = "cheerio-0.20.0.tgz";
      sha1 = "5c710f2bab95653272842ba01c6ea61b3545ec35";
    };
    deps = {
      "css-select-1.2.0" = self.by-version."css-select"."1.2.0";
      "entities-1.1.1" = self.by-version."entities"."1.1.1";
      "htmlparser2-3.8.3" = self.by-version."htmlparser2"."3.8.3";
      "dom-serializer-0.1.0" = self.by-version."dom-serializer"."0.1.0";
      "lodash-4.15.0" = self.by-version."lodash"."4.15.0";
    };
    optionalDependencies = {
      "jsdom-7.2.2" = self.by-version."jsdom"."7.2.2";
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."cheerio"."~0.13.1" =
    self.by-version."cheerio"."0.13.1";
  by-version."cheerio"."0.13.1" = self.buildNodePackage {
    name = "cheerio-0.13.1";
    version = "0.13.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/cheerio/-/cheerio-0.13.1.tgz";
      name = "cheerio-0.13.1.tgz";
      sha1 = "48af1134561b3527f83d9156c4f9a8ebd82b06ec";
    };
    deps = {
      "htmlparser2-3.4.0" = self.by-version."htmlparser2"."3.4.0";
      "underscore-1.5.2" = self.by-version."underscore"."1.5.2";
      "entities-0.5.0" = self.by-version."entities"."0.5.0";
      "CSSselect-0.4.1" = self.by-version."CSSselect"."0.4.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."cheerio"."~0.17.0" =
    self.by-version."cheerio"."0.17.0";
  by-version."cheerio"."0.17.0" = self.buildNodePackage {
    name = "cheerio-0.17.0";
    version = "0.17.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/cheerio/-/cheerio-0.17.0.tgz";
      name = "cheerio-0.17.0.tgz";
      sha1 = "fa5ae42cc60121133d296d0b46d983215f7268ea";
    };
    deps = {
      "CSSselect-0.4.1" = self.by-version."CSSselect"."0.4.1";
      "entities-1.1.1" = self.by-version."entities"."1.1.1";
      "htmlparser2-3.7.3" = self.by-version."htmlparser2"."3.7.3";
      "dom-serializer-0.0.1" = self.by-version."dom-serializer"."0.0.1";
      "lodash-2.4.2" = self.by-version."lodash"."2.4.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."chokidar"."1.4.1" =
    self.by-version."chokidar"."1.4.1";
  by-version."chokidar"."1.4.1" = self.buildNodePackage {
    name = "chokidar-1.4.1";
    version = "1.4.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/chokidar/-/chokidar-1.4.1.tgz";
      name = "chokidar-1.4.1.tgz";
      sha1 = "df1d906769701a0f3df492c37dcc3cb35e6450e4";
    };
    deps = {
      "anymatch-1.3.0" = self.by-version."anymatch"."1.3.0";
      "async-each-0.1.6" = self.by-version."async-each"."0.1.6";
      "glob-parent-2.0.0" = self.by-version."glob-parent"."2.0.0";
      "inherits-2.0.1" = self.by-version."inherits"."2.0.1";
      "is-binary-path-1.0.1" = self.by-version."is-binary-path"."1.0.1";
      "is-glob-2.0.1" = self.by-version."is-glob"."2.0.1";
      "path-is-absolute-1.0.0" = self.by-version."path-is-absolute"."1.0.0";
      "readdirp-2.1.0" = self.by-version."readdirp"."2.1.0";
    };
    optionalDependencies = {
      "fsevents-1.0.14" = self.by-version."fsevents"."1.0.14";
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."clap"."^1.0.9" =
    self.by-version."clap"."1.1.1";
  by-version."clap"."1.1.1" = self.buildNodePackage {
    name = "clap-1.1.1";
    version = "1.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/clap/-/clap-1.1.1.tgz";
      name = "clap-1.1.1.tgz";
      sha1 = "a8a93e0bfb7581ac199c4f001a5525a724ce696d";
    };
    deps = {
      "chalk-1.1.3" = self.by-version."chalk"."1.1.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."clean-css"."3.1.x" =
    self.by-version."clean-css"."3.1.9";
  by-version."clean-css"."3.1.9" = self.buildNodePackage {
    name = "clean-css-3.1.9";
    version = "3.1.9";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/clean-css/-/clean-css-3.1.9.tgz";
      name = "clean-css-3.1.9.tgz";
      sha1 = "dbd05a148be4943bb37ce0679e676cbc9f580266";
    };
    deps = {
      "commander-2.6.0" = self.by-version."commander"."2.6.0";
      "source-map-0.1.43" = self.by-version."source-map"."0.1.43";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."cli"."0.6.x" =
    self.by-version."cli"."0.6.6";
  by-version."cli"."0.6.6" = self.buildNodePackage {
    name = "cli-0.6.6";
    version = "0.6.6";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/cli/-/cli-0.6.6.tgz";
      name = "cli-0.6.6.tgz";
      sha1 = "02ad44a380abf27adac5e6f0cdd7b043d74c53e3";
    };
    deps = {
      "glob-3.2.11" = self.by-version."glob"."3.2.11";
      "exit-0.1.2" = self.by-version."exit"."0.1.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."cli-table"."~0.3.0" =
    self.by-version."cli-table"."0.3.1";
  by-version."cli-table"."0.3.1" = self.buildNodePackage {
    name = "cli-table-0.3.1";
    version = "0.3.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/cli-table/-/cli-table-0.3.1.tgz";
      name = "cli-table-0.3.1.tgz";
      sha1 = "f53b05266a8b1a0b934b3d0821e6e2dc5914ae23";
    };
    deps = {
      "colors-1.0.3" = self.by-version."colors"."1.0.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."cliui"."^2.1.0" =
    self.by-version."cliui"."2.1.0";
  by-version."cliui"."2.1.0" = self.buildNodePackage {
    name = "cliui-2.1.0";
    version = "2.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/cliui/-/cliui-2.1.0.tgz";
      name = "cliui-2.1.0.tgz";
      sha1 = "4b475760ff80264c762c3a1719032e91c7fea0d1";
    };
    deps = {
      "center-align-0.1.3" = self.by-version."center-align"."0.1.3";
      "right-align-0.1.3" = self.by-version."right-align"."0.1.3";
      "wordwrap-0.0.2" = self.by-version."wordwrap"."0.0.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."cliui"."^3.0.3" =
    self.by-version."cliui"."3.2.0";
  by-version."cliui"."3.2.0" = self.buildNodePackage {
    name = "cliui-3.2.0";
    version = "3.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/cliui/-/cliui-3.2.0.tgz";
      name = "cliui-3.2.0.tgz";
      sha1 = "120601537a916d29940f934da3b48d585a39213d";
    };
    deps = {
      "string-width-1.0.2" = self.by-version."string-width"."1.0.2";
      "strip-ansi-3.0.1" = self.by-version."strip-ansi"."3.0.1";
      "wrap-ansi-2.0.0" = self.by-version."wrap-ansi"."2.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."cliui"."^3.2.0" =
    self.by-version."cliui"."3.2.0";
  by-spec."clone"."0.x" =
    self.by-version."clone"."0.2.0";
  by-version."clone"."0.2.0" = self.buildNodePackage {
    name = "clone-0.2.0";
    version = "0.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/clone/-/clone-0.2.0.tgz";
      name = "clone-0.2.0.tgz";
      sha1 = "c6126a90ad4f72dbf5acdb243cc37724fe93fc1f";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."clone"."^0.2.0" =
    self.by-version."clone"."0.2.0";
  by-spec."clone"."^1.0.0" =
    self.by-version."clone"."1.0.2";
  by-version."clone"."1.0.2" = self.buildNodePackage {
    name = "clone-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/clone/-/clone-1.0.2.tgz";
      name = "clone-1.0.2.tgz";
      sha1 = "260b7a99ebb1edfe247538175f783243cb19d149";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."clone"."^1.0.2" =
    self.by-version."clone"."1.0.2";
  by-spec."clone-stats"."^0.0.1" =
    self.by-version."clone-stats"."0.0.1";
  by-version."clone-stats"."0.0.1" = self.buildNodePackage {
    name = "clone-stats-0.0.1";
    version = "0.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/clone-stats/-/clone-stats-0.0.1.tgz";
      name = "clone-stats-0.0.1.tgz";
      sha1 = "b88f94a82cf38b8791d58046ea4029ad88ca99d1";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."clone-stats"."~0.0.1" =
    self.by-version."clone-stats"."0.0.1";
  by-spec."co"."3.1.0" =
    self.by-version."co"."3.1.0";
  by-version."co"."3.1.0" = self.buildNodePackage {
    name = "co-3.1.0";
    version = "3.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/co/-/co-3.1.0.tgz";
      name = "co-3.1.0.tgz";
      sha1 = "4ea54ea5a08938153185e15210c68d9092bc1b78";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."coa"."~1.0.1" =
    self.by-version."coa"."1.0.1";
  by-version."coa"."1.0.1" = self.buildNodePackage {
    name = "coa-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/coa/-/coa-1.0.1.tgz";
      name = "coa-1.0.1.tgz";
      sha1 = "7f959346cfc8719e3f7233cd6852854a7c67d8a3";
    };
    deps = {
      "q-1.4.1" = self.by-version."q"."1.4.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."code-point-at"."^1.0.0" =
    self.by-version."code-point-at"."1.0.0";
  by-version."code-point-at"."1.0.0" = self.buildNodePackage {
    name = "code-point-at-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/code-point-at/-/code-point-at-1.0.0.tgz";
      name = "code-point-at-1.0.0.tgz";
      sha1 = "f69b192d3f7d91e382e4b71bddb77878619ab0c6";
    };
    deps = {
      "number-is-nan-1.0.0" = self.by-version."number-is-nan"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."colors"."1.0.3" =
    self.by-version."colors"."1.0.3";
  by-version."colors"."1.0.3" = self.buildNodePackage {
    name = "colors-1.0.3";
    version = "1.0.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/colors/-/colors-1.0.3.tgz";
      name = "colors-1.0.3.tgz";
      sha1 = "0433f44d809680fdeb60ed260f1b0c262e82a40b";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."colors"."~1.1.2" =
    self.by-version."colors"."1.1.2";
  by-version."colors"."1.1.2" = self.buildNodePackage {
    name = "colors-1.1.2";
    version = "1.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/colors/-/colors-1.1.2.tgz";
      name = "colors-1.1.2.tgz";
      sha1 = "168a4701756b6a7f51a12ce0c97bfa28c084ed63";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "colors" = self.by-version."colors"."1.1.2";
  by-spec."combined-stream"."^1.0.5" =
    self.by-version."combined-stream"."1.0.5";
  by-version."combined-stream"."1.0.5" = self.buildNodePackage {
    name = "combined-stream-1.0.5";
    version = "1.0.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/combined-stream/-/combined-stream-1.0.5.tgz";
      name = "combined-stream-1.0.5.tgz";
      sha1 = "938370a57b4a51dea2c77c15d5c5fdf895164009";
    };
    deps = {
      "delayed-stream-1.0.0" = self.by-version."delayed-stream"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."combined-stream"."~0.0.4" =
    self.by-version."combined-stream"."0.0.7";
  by-version."combined-stream"."0.0.7" = self.buildNodePackage {
    name = "combined-stream-0.0.7";
    version = "0.0.7";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/combined-stream/-/combined-stream-0.0.7.tgz";
      name = "combined-stream-0.0.7.tgz";
      sha1 = "0137e657baa5a7541c57ac37ac5fc07d73b4dc1f";
    };
    deps = {
      "delayed-stream-0.0.5" = self.by-version."delayed-stream"."0.0.5";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."combined-stream"."~1.0.5" =
    self.by-version."combined-stream"."1.0.5";
  by-spec."commander"."2.6.x" =
    self.by-version."commander"."2.6.0";
  by-version."commander"."2.6.0" = self.buildNodePackage {
    name = "commander-2.6.0";
    version = "2.6.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/commander/-/commander-2.6.0.tgz";
      name = "commander-2.6.0.tgz";
      sha1 = "9df7e52fb2a0cb0fb89058ee80c3104225f37e1d";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."commander"."^2.2.0" =
    self.by-version."commander"."2.9.0";
  by-version."commander"."2.9.0" = self.buildNodePackage {
    name = "commander-2.9.0";
    version = "2.9.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/commander/-/commander-2.9.0.tgz";
      name = "commander-2.9.0.tgz";
      sha1 = "9c99094176e12240cb22d6c5146098400fe0f7d4";
    };
    deps = {
      "graceful-readlink-1.0.1" = self.by-version."graceful-readlink"."1.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."commander"."^2.9.0" =
    self.by-version."commander"."2.9.0";
  by-spec."commander"."~2.8.1" =
    self.by-version."commander"."2.8.1";
  by-version."commander"."2.8.1" = self.buildNodePackage {
    name = "commander-2.8.1";
    version = "2.8.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/commander/-/commander-2.8.1.tgz";
      name = "commander-2.8.1.tgz";
      sha1 = "06be367febfda0c330aa1e2a072d3dc9762425d4";
    };
    deps = {
      "graceful-readlink-1.0.1" = self.by-version."graceful-readlink"."1.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."component-bind"."1.0.0" =
    self.by-version."component-bind"."1.0.0";
  by-version."component-bind"."1.0.0" = self.buildNodePackage {
    name = "component-bind-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/component-bind/-/component-bind-1.0.0.tgz";
      name = "component-bind-1.0.0.tgz";
      sha1 = "00c608ab7dcd93897c0009651b1d3a8e1e73bbd1";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."component-emitter"."1.1.2" =
    self.by-version."component-emitter"."1.1.2";
  by-version."component-emitter"."1.1.2" = self.buildNodePackage {
    name = "component-emitter-1.1.2";
    version = "1.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/component-emitter/-/component-emitter-1.1.2.tgz";
      name = "component-emitter-1.1.2.tgz";
      sha1 = "296594f2753daa63996d2af08d15a95116c9aec3";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."component-emitter"."1.2.0" =
    self.by-version."component-emitter"."1.2.0";
  by-version."component-emitter"."1.2.0" = self.buildNodePackage {
    name = "component-emitter-1.2.0";
    version = "1.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/component-emitter/-/component-emitter-1.2.0.tgz";
      name = "component-emitter-1.2.0.tgz";
      sha1 = "ccd113a86388d06482d03de3fc7df98526ba8efe";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."component-emitter"."~1.2.0" =
    self.by-version."component-emitter"."1.2.1";
  by-version."component-emitter"."1.2.1" = self.buildNodePackage {
    name = "component-emitter-1.2.1";
    version = "1.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/component-emitter/-/component-emitter-1.2.1.tgz";
      name = "component-emitter-1.2.1.tgz";
      sha1 = "137918d6d78283f7df7a6b7c5a63e140e69425e6";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."component-inherit"."0.0.3" =
    self.by-version."component-inherit"."0.0.3";
  by-version."component-inherit"."0.0.3" = self.buildNodePackage {
    name = "component-inherit-0.0.3";
    version = "0.0.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/component-inherit/-/component-inherit-0.0.3.tgz";
      name = "component-inherit-0.0.3.tgz";
      sha1 = "645fc4adf58b72b649d5cae65135619db26ff143";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."concat-map"."0.0.1" =
    self.by-version."concat-map"."0.0.1";
  by-version."concat-map"."0.0.1" = self.buildNodePackage {
    name = "concat-map-0.0.1";
    version = "0.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/concat-map/-/concat-map-0.0.1.tgz";
      name = "concat-map-0.0.1.tgz";
      sha1 = "d8a96bd77fd68df7793a73036a3ba0d5405d477b";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."concat-stream"."1.4.x" =
    self.by-version."concat-stream"."1.4.10";
  by-version."concat-stream"."1.4.10" = self.buildNodePackage {
    name = "concat-stream-1.4.10";
    version = "1.4.10";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/concat-stream/-/concat-stream-1.4.10.tgz";
      name = "concat-stream-1.4.10.tgz";
      sha1 = "acc3bbf5602cb8cc980c6ac840fa7d8603e3ef36";
    };
    deps = {
      "inherits-2.0.1" = self.by-version."inherits"."2.0.1";
      "typedarray-0.0.6" = self.by-version."typedarray"."0.0.6";
      "readable-stream-1.1.14" = self.by-version."readable-stream"."1.1.14";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."concat-stream"."^1.4.6" =
    self.by-version."concat-stream"."1.5.1";
  by-version."concat-stream"."1.5.1" = self.buildNodePackage {
    name = "concat-stream-1.5.1";
    version = "1.5.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/concat-stream/-/concat-stream-1.5.1.tgz";
      name = "concat-stream-1.5.1.tgz";
      sha1 = "f3b80acf9e1f48e3875c0688b41b6c31602eea1c";
    };
    deps = {
      "inherits-2.0.1" = self.by-version."inherits"."2.0.1";
      "typedarray-0.0.6" = self.by-version."typedarray"."0.0.6";
      "readable-stream-2.0.6" = self.by-version."readable-stream"."2.0.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."concat-stream"."^1.4.7" =
    self.by-version."concat-stream"."1.5.1";
  by-spec."connect"."1.x" =
    self.by-version."connect"."1.9.2";
  by-version."connect"."1.9.2" = self.buildNodePackage {
    name = "connect-1.9.2";
    version = "1.9.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/connect/-/connect-1.9.2.tgz";
      name = "connect-1.9.2.tgz";
      sha1 = "42880a22e9438ae59a8add74e437f58ae8e52807";
    };
    deps = {
      "qs-6.2.1" = self.by-version."qs"."6.2.1";
      "mime-1.3.4" = self.by-version."mime"."1.3.4";
      "formidable-1.0.17" = self.by-version."formidable"."1.0.17";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."connect"."^3.3.5" =
    self.by-version."connect"."3.4.1";
  by-version."connect"."3.4.1" = self.buildNodePackage {
    name = "connect-3.4.1";
    version = "3.4.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/connect/-/connect-3.4.1.tgz";
      name = "connect-3.4.1.tgz";
      sha1 = "a21361d3f4099ef761cda6dc4a973bb1ebb0a34d";
    };
    deps = {
      "debug-2.2.0" = self.by-version."debug"."2.2.0";
      "finalhandler-0.4.1" = self.by-version."finalhandler"."0.4.1";
      "parseurl-1.3.1" = self.by-version."parseurl"."1.3.1";
      "utils-merge-1.0.0" = self.by-version."utils-merge"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."connect"."^3.4.0" =
    self.by-version."connect"."3.4.1";
  by-spec."connect-history-api-fallback"."^1.1.0" =
    self.by-version."connect-history-api-fallback"."1.3.0";
  by-version."connect-history-api-fallback"."1.3.0" = self.buildNodePackage {
    name = "connect-history-api-fallback-1.3.0";
    version = "1.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/connect-history-api-fallback/-/connect-history-api-fallback-1.3.0.tgz";
      name = "connect-history-api-fallback-1.3.0.tgz";
      sha1 = "e51d17f8f0ef0db90a64fdb47de3051556e9f169";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."console-control-strings"."^1.0.0" =
    self.by-version."console-control-strings"."1.1.0";
  by-version."console-control-strings"."1.1.0" = self.buildNodePackage {
    name = "console-control-strings-1.1.0";
    version = "1.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/console-control-strings/-/console-control-strings-1.1.0.tgz";
      name = "console-control-strings-1.1.0.tgz";
      sha1 = "3d7cf4464db6446ea644bf4b39507f9851008e8e";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."console-control-strings"."~1.1.0" =
    self.by-version."console-control-strings"."1.1.0";
  by-spec."console-stream"."^0.1.1" =
    self.by-version."console-stream"."0.1.1";
  by-version."console-stream"."0.1.1" = self.buildNodePackage {
    name = "console-stream-0.1.1";
    version = "0.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/console-stream/-/console-stream-0.1.1.tgz";
      name = "console-stream-0.1.1.tgz";
      sha1 = "a095fe07b20465955f2fafd28b5d72bccd949d44";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."constant-case"."^1.1.0" =
    self.by-version."constant-case"."1.1.2";
  by-version."constant-case"."1.1.2" = self.buildNodePackage {
    name = "constant-case-1.1.2";
    version = "1.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/constant-case/-/constant-case-1.1.2.tgz";
      name = "constant-case-1.1.2.tgz";
      sha1 = "8ec2ca5ba343e00aa38dbf4e200fd5ac907efd63";
    };
    deps = {
      "snake-case-1.1.2" = self.by-version."snake-case"."1.1.2";
      "upper-case-1.1.3" = self.by-version."upper-case"."1.1.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."convert-source-map"."^1.1.0" =
    self.by-version."convert-source-map"."1.3.0";
  by-version."convert-source-map"."1.3.0" = self.buildNodePackage {
    name = "convert-source-map-1.3.0";
    version = "1.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/convert-source-map/-/convert-source-map-1.3.0.tgz";
      name = "convert-source-map-1.3.0.tgz";
      sha1 = "e9f3e9c6e2728efc2676696a70eb382f73106a67";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."convert-source-map"."^1.1.1" =
    self.by-version."convert-source-map"."1.3.0";
  by-spec."cookiejar"."2.0.6" =
    self.by-version."cookiejar"."2.0.6";
  by-version."cookiejar"."2.0.6" = self.buildNodePackage {
    name = "cookiejar-2.0.6";
    version = "2.0.6";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/cookiejar/-/cookiejar-2.0.6.tgz";
      name = "cookiejar-2.0.6.tgz";
      sha1 = "0abf356ad00d1c5a219d88d44518046dd026acfe";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."core-js"."^1.0.0" =
    self.by-version."core-js"."1.2.7";
  by-version."core-js"."1.2.7" = self.buildNodePackage {
    name = "core-js-1.2.7";
    version = "1.2.7";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/core-js/-/core-js-1.2.7.tgz";
      name = "core-js-1.2.7.tgz";
      sha1 = "652294c14651db28fa93bd2d5ff2983a4f08c636";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."core-js"."^2.1.0" =
    self.by-version."core-js"."2.4.1";
  by-version."core-js"."2.4.1" = self.buildNodePackage {
    name = "core-js-2.4.1";
    version = "2.4.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/core-js/-/core-js-2.4.1.tgz";
      name = "core-js-2.4.1.tgz";
      sha1 = "4de911e667b0eae9124e34254b53aea6fc618d3e";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."core-js"."^2.4.0" =
    self.by-version."core-js"."2.4.1";
  by-spec."core-util-is"."~1.0.0" =
    self.by-version."core-util-is"."1.0.2";
  by-version."core-util-is"."1.0.2" = self.buildNodePackage {
    name = "core-util-is-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/core-util-is/-/core-util-is-1.0.2.tgz";
      name = "core-util-is-1.0.2.tgz";
      sha1 = "b5fd54220aa2bc5ab57aab7140c940754503c1a7";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."create-error-class"."^3.0.1" =
    self.by-version."create-error-class"."3.0.2";
  by-version."create-error-class"."3.0.2" = self.buildNodePackage {
    name = "create-error-class-3.0.2";
    version = "3.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/create-error-class/-/create-error-class-3.0.2.tgz";
      name = "create-error-class-3.0.2.tgz";
      sha1 = "06be7abef947a3f14a30fd610671d401bca8b7b6";
    };
    deps = {
      "capture-stack-trace-1.0.0" = self.by-version."capture-stack-trace"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."cross-spawn"."^3.0.0" =
    self.by-version."cross-spawn"."3.0.1";
  by-version."cross-spawn"."3.0.1" = self.buildNodePackage {
    name = "cross-spawn-3.0.1";
    version = "3.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/cross-spawn/-/cross-spawn-3.0.1.tgz";
      name = "cross-spawn-3.0.1.tgz";
      sha1 = "1256037ecb9f0c5f79e3d6ef135e30770184b982";
    };
    deps = {
      "lru-cache-4.0.1" = self.by-version."lru-cache"."4.0.1";
      "which-1.2.10" = self.by-version."which"."1.2.10";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."cryptiles"."0.2.x" =
    self.by-version."cryptiles"."0.2.2";
  by-version."cryptiles"."0.2.2" = self.buildNodePackage {
    name = "cryptiles-0.2.2";
    version = "0.2.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/cryptiles/-/cryptiles-0.2.2.tgz";
      name = "cryptiles-0.2.2.tgz";
      sha1 = "ed91ff1f17ad13d3748288594f8a48a0d26f325c";
    };
    deps = {
      "boom-0.4.2" = self.by-version."boom"."0.4.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."cryptiles"."2.x.x" =
    self.by-version."cryptiles"."2.0.5";
  by-version."cryptiles"."2.0.5" = self.buildNodePackage {
    name = "cryptiles-2.0.5";
    version = "2.0.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/cryptiles/-/cryptiles-2.0.5.tgz";
      name = "cryptiles-2.0.5.tgz";
      sha1 = "3bdfecdc608147c1c67202fa291e7dca59eaa3b8";
    };
    deps = {
      "boom-2.10.1" = self.by-version."boom"."2.10.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."css"."^2.0.0" =
    self.by-version."css"."2.2.1";
  by-version."css"."2.2.1" = self.buildNodePackage {
    name = "css-2.2.1";
    version = "2.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/css/-/css-2.2.1.tgz";
      name = "css-2.2.1.tgz";
      sha1 = "73a4c81de85db664d4ee674f7d47085e3b2d55dc";
    };
    deps = {
      "source-map-0.1.43" = self.by-version."source-map"."0.1.43";
      "source-map-resolve-0.3.1" = self.by-version."source-map-resolve"."0.3.1";
      "urix-0.1.0" = self.by-version."urix"."0.1.0";
      "inherits-2.0.1" = self.by-version."inherits"."2.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."css-parse"."^2.0.0" =
    self.by-version."css-parse"."2.0.0";
  by-version."css-parse"."2.0.0" = self.buildNodePackage {
    name = "css-parse-2.0.0";
    version = "2.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/css-parse/-/css-parse-2.0.0.tgz";
      name = "css-parse-2.0.0.tgz";
      sha1 = "a468ee667c16d81ccf05c58c38d2a97c780dbfd4";
    };
    deps = {
      "css-2.2.1" = self.by-version."css"."2.2.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."css-property"."^2.0.0" =
    self.by-version."css-property"."2.0.0";
  by-version."css-property"."2.0.0" = self.buildNodePackage {
    name = "css-property-2.0.0";
    version = "2.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/css-property/-/css-property-2.0.0.tgz";
      name = "css-property-2.0.0.tgz";
      sha1 = "39b772471a69d48a4f494f601e5e03b414c81d96";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."css-rules"."^1.0.0" =
    self.by-version."css-rules"."1.0.2";
  by-version."css-rules"."1.0.2" = self.buildNodePackage {
    name = "css-rules-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/css-rules/-/css-rules-1.0.2.tgz";
      name = "css-rules-1.0.2.tgz";
      sha1 = "f2d05444c0927da09e46d1ca695af81a6fc525d9";
    };
    deps = {
      "cssom-0.3.1" = self.by-version."cssom"."0.3.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."css-select"."~1.0.0" =
    self.by-version."css-select"."1.0.0";
  by-version."css-select"."1.0.0" = self.buildNodePackage {
    name = "css-select-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/css-select/-/css-select-1.0.0.tgz";
      name = "css-select-1.0.0.tgz";
      sha1 = "b1121ca51848dd264e2244d058cee254deeb44b0";
    };
    deps = {
      "css-what-1.0.0" = self.by-version."css-what"."1.0.0";
      "domutils-1.4.3" = self.by-version."domutils"."1.4.3";
      "boolbase-1.0.0" = self.by-version."boolbase"."1.0.0";
      "nth-check-1.0.1" = self.by-version."nth-check"."1.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."css-select"."~1.2.0" =
    self.by-version."css-select"."1.2.0";
  by-version."css-select"."1.2.0" = self.buildNodePackage {
    name = "css-select-1.2.0";
    version = "1.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/css-select/-/css-select-1.2.0.tgz";
      name = "css-select-1.2.0.tgz";
      sha1 = "2b3a110539c5355f1cd8d314623e870b121ec858";
    };
    deps = {
      "css-what-2.1.0" = self.by-version."css-what"."2.1.0";
      "domutils-1.5.1" = self.by-version."domutils"."1.5.1";
      "boolbase-1.0.0" = self.by-version."boolbase"."1.0.0";
      "nth-check-1.0.1" = self.by-version."nth-check"."1.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."css-stringify"."^2.0.0" =
    self.by-version."css-stringify"."2.0.0";
  by-version."css-stringify"."2.0.0" = self.buildNodePackage {
    name = "css-stringify-2.0.0";
    version = "2.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/css-stringify/-/css-stringify-2.0.0.tgz";
      name = "css-stringify-2.0.0.tgz";
      sha1 = "2ef33dcf498c3d3ee82bd73d0a01ac28628cd0fa";
    };
    deps = {
      "css-2.2.1" = self.by-version."css"."2.2.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."css-what"."1.0" =
    self.by-version."css-what"."1.0.0";
  by-version."css-what"."1.0.0" = self.buildNodePackage {
    name = "css-what-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/css-what/-/css-what-1.0.0.tgz";
      name = "css-what-1.0.0.tgz";
      sha1 = "d7cc2df45180666f99d2b14462639469e00f736c";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."css-what"."2.1" =
    self.by-version."css-what"."2.1.0";
  by-version."css-what"."2.1.0" = self.buildNodePackage {
    name = "css-what-2.1.0";
    version = "2.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/css-what/-/css-what-2.1.0.tgz";
      name = "css-what-2.1.0.tgz";
      sha1 = "9467d032c38cfaefb9f2d79501253062f87fa1bd";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."csso"."~2.0.0" =
    self.by-version."csso"."2.0.0";
  by-version."csso"."2.0.0" = self.buildNodePackage {
    name = "csso-2.0.0";
    version = "2.0.0";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/csso/-/csso-2.0.0.tgz";
      name = "csso-2.0.0.tgz";
      sha1 = "178b43a44621221c27756086f531e02f42900ee8";
    };
    deps = {
      "clap-1.1.1" = self.by-version."clap"."1.1.1";
      "source-map-0.5.6" = self.by-version."source-map"."0.5.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."cssom"."0.3.x" =
    self.by-version."cssom"."0.3.1";
  by-version."cssom"."0.3.1" = self.buildNodePackage {
    name = "cssom-0.3.1";
    version = "0.3.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/cssom/-/cssom-0.3.1.tgz";
      name = "cssom-0.3.1.tgz";
      sha1 = "c9e37ef2490e64f6d1baa10fda852257082c25d3";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."cssom".">= 0.3.0 < 0.4.0" =
    self.by-version."cssom"."0.3.1";
  by-spec."cssom"."^0.3.0" =
    self.by-version."cssom"."0.3.1";
  by-spec."cssstyle".">= 0.2.29 < 0.3.0" =
    self.by-version."cssstyle"."0.2.37";
  by-version."cssstyle"."0.2.37" = self.buildNodePackage {
    name = "cssstyle-0.2.37";
    version = "0.2.37";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/cssstyle/-/cssstyle-0.2.37.tgz";
      name = "cssstyle-0.2.37.tgz";
      sha1 = "541097234cb2513c83ceed3acddc27ff27987d54";
    };
    deps = {
      "cssom-0.3.1" = self.by-version."cssom"."0.3.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."ctype"."0.5.3" =
    self.by-version."ctype"."0.5.3";
  by-version."ctype"."0.5.3" = self.buildNodePackage {
    name = "ctype-0.5.3";
    version = "0.5.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/ctype/-/ctype-0.5.3.tgz";
      name = "ctype-0.5.3.tgz";
      sha1 = "82c18c2461f74114ef16c135224ad0b9144ca12f";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."currently-unhandled"."^0.4.1" =
    self.by-version."currently-unhandled"."0.4.1";
  by-version."currently-unhandled"."0.4.1" = self.buildNodePackage {
    name = "currently-unhandled-0.4.1";
    version = "0.4.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/currently-unhandled/-/currently-unhandled-0.4.1.tgz";
      name = "currently-unhandled-0.4.1.tgz";
      sha1 = "988df33feab191ef799a61369dd76c17adf957ea";
    };
    deps = {
      "array-find-index-1.0.1" = self.by-version."array-find-index"."1.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."d"."^0.1.1" =
    self.by-version."d"."0.1.1";
  by-version."d"."0.1.1" = self.buildNodePackage {
    name = "d-0.1.1";
    version = "0.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/d/-/d-0.1.1.tgz";
      name = "d-0.1.1.tgz";
      sha1 = "da184c535d18d8ee7ba2aa229b914009fae11309";
    };
    deps = {
      "es5-ext-0.10.12" = self.by-version."es5-ext"."0.10.12";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."d"."~0.1.1" =
    self.by-version."d"."0.1.1";
  by-spec."dashdash"."^1.12.0" =
    self.by-version."dashdash"."1.14.0";
  by-version."dashdash"."1.14.0" = self.buildNodePackage {
    name = "dashdash-1.14.0";
    version = "1.14.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/dashdash/-/dashdash-1.14.0.tgz";
      name = "dashdash-1.14.0.tgz";
      sha1 = "29e486c5418bf0f356034a993d51686a33e84141";
    };
    deps = {
      "assert-plus-1.0.0" = self.by-version."assert-plus"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."dateformat"."^1.0.11" =
    self.by-version."dateformat"."1.0.12";
  by-version."dateformat"."1.0.12" = self.buildNodePackage {
    name = "dateformat-1.0.12";
    version = "1.0.12";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/dateformat/-/dateformat-1.0.12.tgz";
      name = "dateformat-1.0.12.tgz";
      sha1 = "9f124b67594c937ff706932e4a642cca8dbbfee9";
    };
    deps = {
      "get-stdin-4.0.1" = self.by-version."get-stdin"."4.0.1";
      "meow-3.7.0" = self.by-version."meow"."3.7.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."dateformat"."^1.0.7-1.2.3" =
    self.by-version."dateformat"."1.0.12";
  by-spec."dateformat"."~1.0.8-1.2.3" =
    self.by-version."dateformat"."1.0.12";
  by-spec."debug"."0.7.4" =
    self.by-version."debug"."0.7.4";
  by-version."debug"."0.7.4" = self.buildNodePackage {
    name = "debug-0.7.4";
    version = "0.7.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/debug/-/debug-0.7.4.tgz";
      name = "debug-0.7.4.tgz";
      sha1 = "06e1ea8082c2cb14e39806e22e2f6f757f92af39";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."debug"."2" =
    self.by-version."debug"."2.2.0";
  by-version."debug"."2.2.0" = self.buildNodePackage {
    name = "debug-2.2.0";
    version = "2.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/debug/-/debug-2.2.0.tgz";
      name = "debug-2.2.0.tgz";
      sha1 = "f87057e995b1a1f6ae6a4960664137bc56f039da";
    };
    deps = {
      "ms-0.7.1" = self.by-version."ms"."0.7.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."debug"."2.2.0" =
    self.by-version."debug"."2.2.0";
  by-spec."debug"."^2.1.1" =
    self.by-version."debug"."2.2.0";
  by-spec."debug"."^2.2.0" =
    self.by-version."debug"."2.2.0";
  by-spec."debug"."~2.2.0" =
    self.by-version."debug"."2.2.0";
  by-spec."decamelize"."^1.0.0" =
    self.by-version."decamelize"."1.2.0";
  by-version."decamelize"."1.2.0" = self.buildNodePackage {
    name = "decamelize-1.2.0";
    version = "1.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/decamelize/-/decamelize-1.2.0.tgz";
      name = "decamelize-1.2.0.tgz";
      sha1 = "f6534d15148269b20352e7bee26f501f9a191290";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."decamelize"."^1.1.1" =
    self.by-version."decamelize"."1.2.0";
  by-spec."decamelize"."^1.1.2" =
    self.by-version."decamelize"."1.2.0";
  by-spec."decompress"."^3.0.0" =
    self.by-version."decompress"."3.0.0";
  by-version."decompress"."3.0.0" = self.buildNodePackage {
    name = "decompress-3.0.0";
    version = "3.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/decompress/-/decompress-3.0.0.tgz";
      name = "decompress-3.0.0.tgz";
      sha1 = "af1dd50d06e3bfc432461d37de11b38c0d991bed";
    };
    deps = {
      "buffer-to-vinyl-1.1.0" = self.by-version."buffer-to-vinyl"."1.1.0";
      "concat-stream-1.5.1" = self.by-version."concat-stream"."1.5.1";
      "decompress-tar-3.1.0" = self.by-version."decompress-tar"."3.1.0";
      "decompress-tarbz2-3.1.0" = self.by-version."decompress-tarbz2"."3.1.0";
      "decompress-targz-3.1.0" = self.by-version."decompress-targz"."3.1.0";
      "decompress-unzip-3.4.0" = self.by-version."decompress-unzip"."3.4.0";
      "stream-combiner2-1.1.1" = self.by-version."stream-combiner2"."1.1.1";
      "vinyl-assign-1.2.1" = self.by-version."vinyl-assign"."1.2.1";
      "vinyl-fs-2.4.3" = self.by-version."vinyl-fs"."2.4.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."decompress-tar"."^3.0.0" =
    self.by-version."decompress-tar"."3.1.0";
  by-version."decompress-tar"."3.1.0" = self.buildNodePackage {
    name = "decompress-tar-3.1.0";
    version = "3.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/decompress-tar/-/decompress-tar-3.1.0.tgz";
      name = "decompress-tar-3.1.0.tgz";
      sha1 = "217c789f9b94450efaadc5c5e537978fc333c466";
    };
    deps = {
      "is-tar-1.0.0" = self.by-version."is-tar"."1.0.0";
      "object-assign-2.1.1" = self.by-version."object-assign"."2.1.1";
      "strip-dirs-1.1.1" = self.by-version."strip-dirs"."1.1.1";
      "tar-stream-1.5.2" = self.by-version."tar-stream"."1.5.2";
      "through2-0.6.5" = self.by-version."through2"."0.6.5";
      "vinyl-0.4.6" = self.by-version."vinyl"."0.4.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."decompress-tarbz2"."^3.0.0" =
    self.by-version."decompress-tarbz2"."3.1.0";
  by-version."decompress-tarbz2"."3.1.0" = self.buildNodePackage {
    name = "decompress-tarbz2-3.1.0";
    version = "3.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/decompress-tarbz2/-/decompress-tarbz2-3.1.0.tgz";
      name = "decompress-tarbz2-3.1.0.tgz";
      sha1 = "8b23935681355f9f189d87256a0f8bdd96d9666d";
    };
    deps = {
      "is-bzip2-1.0.0" = self.by-version."is-bzip2"."1.0.0";
      "object-assign-2.1.1" = self.by-version."object-assign"."2.1.1";
      "seek-bzip-1.0.5" = self.by-version."seek-bzip"."1.0.5";
      "strip-dirs-1.1.1" = self.by-version."strip-dirs"."1.1.1";
      "tar-stream-1.5.2" = self.by-version."tar-stream"."1.5.2";
      "through2-0.6.5" = self.by-version."through2"."0.6.5";
      "vinyl-0.4.6" = self.by-version."vinyl"."0.4.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."decompress-targz"."^3.0.0" =
    self.by-version."decompress-targz"."3.1.0";
  by-version."decompress-targz"."3.1.0" = self.buildNodePackage {
    name = "decompress-targz-3.1.0";
    version = "3.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/decompress-targz/-/decompress-targz-3.1.0.tgz";
      name = "decompress-targz-3.1.0.tgz";
      sha1 = "b2c13df98166268991b715d6447f642e9696f5a0";
    };
    deps = {
      "is-gzip-1.0.0" = self.by-version."is-gzip"."1.0.0";
      "object-assign-2.1.1" = self.by-version."object-assign"."2.1.1";
      "strip-dirs-1.1.1" = self.by-version."strip-dirs"."1.1.1";
      "tar-stream-1.5.2" = self.by-version."tar-stream"."1.5.2";
      "through2-0.6.5" = self.by-version."through2"."0.6.5";
      "vinyl-0.4.6" = self.by-version."vinyl"."0.4.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."decompress-unzip"."^3.0.0" =
    self.by-version."decompress-unzip"."3.4.0";
  by-version."decompress-unzip"."3.4.0" = self.buildNodePackage {
    name = "decompress-unzip-3.4.0";
    version = "3.4.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/decompress-unzip/-/decompress-unzip-3.4.0.tgz";
      name = "decompress-unzip-3.4.0.tgz";
      sha1 = "61475b4152066bbe3fee12f9d629d15fe6478eeb";
    };
    deps = {
      "is-zip-1.0.0" = self.by-version."is-zip"."1.0.0";
      "read-all-stream-3.1.0" = self.by-version."read-all-stream"."3.1.0";
      "stat-mode-0.2.1" = self.by-version."stat-mode"."0.2.1";
      "strip-dirs-1.1.1" = self.by-version."strip-dirs"."1.1.1";
      "through2-2.0.1" = self.by-version."through2"."2.0.1";
      "vinyl-1.2.0" = self.by-version."vinyl"."1.2.0";
      "yauzl-2.6.0" = self.by-version."yauzl"."2.6.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."deep-extend"."~0.4.0" =
    self.by-version."deep-extend"."0.4.1";
  by-version."deep-extend"."0.4.1" = self.buildNodePackage {
    name = "deep-extend-0.4.1";
    version = "0.4.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/deep-extend/-/deep-extend-0.4.1.tgz";
      name = "deep-extend-0.4.1.tgz";
      sha1 = "efe4113d08085f4e6f9687759810f807469e2253";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."deep-is"."~0.1.3" =
    self.by-version."deep-is"."0.1.3";
  by-version."deep-is"."0.1.3" = self.buildNodePackage {
    name = "deep-is-0.1.3";
    version = "0.1.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/deep-is/-/deep-is-0.1.3.tgz";
      name = "deep-is-0.1.3.tgz";
      sha1 = "b369d6fb5dbc13eecf524f91b070feedc357cf34";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."deepmerge"."^0.2.10" =
    self.by-version."deepmerge"."0.2.10";
  by-version."deepmerge"."0.2.10" = self.buildNodePackage {
    name = "deepmerge-0.2.10";
    version = "0.2.10";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/deepmerge/-/deepmerge-0.2.10.tgz";
      name = "deepmerge-0.2.10.tgz";
      sha1 = "8906bf9e525a4fbf1b203b2afcb4640249821219";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."defaults"."^1.0.0" =
    self.by-version."defaults"."1.0.3";
  by-version."defaults"."1.0.3" = self.buildNodePackage {
    name = "defaults-1.0.3";
    version = "1.0.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/defaults/-/defaults-1.0.3.tgz";
      name = "defaults-1.0.3.tgz";
      sha1 = "c656051e9817d9ff08ed881477f3fe4019f3ef7d";
    };
    deps = {
      "clone-1.0.2" = self.by-version."clone"."1.0.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."delayed-stream"."0.0.5" =
    self.by-version."delayed-stream"."0.0.5";
  by-version."delayed-stream"."0.0.5" = self.buildNodePackage {
    name = "delayed-stream-0.0.5";
    version = "0.0.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/delayed-stream/-/delayed-stream-0.0.5.tgz";
      name = "delayed-stream-0.0.5.tgz";
      sha1 = "d4b1f43a93e8296dfe02694f4680bc37a313c73f";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."delayed-stream"."~1.0.0" =
    self.by-version."delayed-stream"."1.0.0";
  by-version."delayed-stream"."1.0.0" = self.buildNodePackage {
    name = "delayed-stream-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/delayed-stream/-/delayed-stream-1.0.0.tgz";
      name = "delayed-stream-1.0.0.tgz";
      sha1 = "df3ae199acadfb7d440aaae0b29e2272b24ec619";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."delegates"."^1.0.0" =
    self.by-version."delegates"."1.0.0";
  by-version."delegates"."1.0.0" = self.buildNodePackage {
    name = "delegates-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/delegates/-/delegates-1.0.0.tgz";
      name = "delegates-1.0.0.tgz";
      sha1 = "84c6e159b81904fdca59a0ef44cd870d31250f9a";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."depd"."~1.1.0" =
    self.by-version."depd"."1.1.0";
  by-version."depd"."1.1.0" = self.buildNodePackage {
    name = "depd-1.1.0";
    version = "1.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/depd/-/depd-1.1.0.tgz";
      name = "depd-1.1.0.tgz";
      sha1 = "e1bd82c6aab6ced965b97b88b17ed3e528ca18c3";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."deprecated"."^0.0.1" =
    self.by-version."deprecated"."0.0.1";
  by-version."deprecated"."0.0.1" = self.buildNodePackage {
    name = "deprecated-0.0.1";
    version = "0.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/deprecated/-/deprecated-0.0.1.tgz";
      name = "deprecated-0.0.1.tgz";
      sha1 = "f9c9af5464afa1e7a971458a8bdef2aa94d5bb19";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."destroy"."~1.0.4" =
    self.by-version."destroy"."1.0.4";
  by-version."destroy"."1.0.4" = self.buildNodePackage {
    name = "destroy-1.0.4";
    version = "1.0.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/destroy/-/destroy-1.0.4.tgz";
      name = "destroy-1.0.4.tgz";
      sha1 = "978857442c44749e4206613e37946205826abd80";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."detect-file"."^0.1.0" =
    self.by-version."detect-file"."0.1.0";
  by-version."detect-file"."0.1.0" = self.buildNodePackage {
    name = "detect-file-0.1.0";
    version = "0.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/detect-file/-/detect-file-0.1.0.tgz";
      name = "detect-file-0.1.0.tgz";
      sha1 = "4935dedfd9488648e006b0129566e9386711ea63";
    };
    deps = {
      "fs-exists-sync-0.1.0" = self.by-version."fs-exists-sync"."0.1.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."detect-indent"."^3.0.1" =
    self.by-version."detect-indent"."3.0.1";
  by-version."detect-indent"."3.0.1" = self.buildNodePackage {
    name = "detect-indent-3.0.1";
    version = "3.0.1";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/detect-indent/-/detect-indent-3.0.1.tgz";
      name = "detect-indent-3.0.1.tgz";
      sha1 = "9dc5e5ddbceef8325764b9451b02bc6d54084f75";
    };
    deps = {
      "get-stdin-4.0.1" = self.by-version."get-stdin"."4.0.1";
      "minimist-1.2.0" = self.by-version."minimist"."1.2.0";
      "repeating-1.1.3" = self.by-version."repeating"."1.1.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."dev-ip"."^1.0.1" =
    self.by-version."dev-ip"."1.0.1";
  by-version."dev-ip"."1.0.1" = self.buildNodePackage {
    name = "dev-ip-1.0.1";
    version = "1.0.1";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/dev-ip/-/dev-ip-1.0.1.tgz";
      name = "dev-ip-1.0.1.tgz";
      sha1 = "a76a3ed1855be7a012bb8ac16cb80f3c00dc28f0";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."directmail"."~0.1.7" =
    self.by-version."directmail"."0.1.8";
  by-version."directmail"."0.1.8" = self.buildNodePackage {
    name = "directmail-0.1.8";
    version = "0.1.8";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/directmail/-/directmail-0.1.8.tgz";
      name = "directmail-0.1.8.tgz";
      sha1 = "e4852c8a0c5519bef4904fcd96d760822f42a446";
    };
    deps = {
      "simplesmtp-0.3.35" = self.by-version."simplesmtp"."0.3.35";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."dkim-signer"."~0.1.1" =
    self.by-version."dkim-signer"."0.1.2";
  by-version."dkim-signer"."0.1.2" = self.buildNodePackage {
    name = "dkim-signer-0.1.2";
    version = "0.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/dkim-signer/-/dkim-signer-0.1.2.tgz";
      name = "dkim-signer-0.1.2.tgz";
      sha1 = "2ff5d61c87d8fbff5a8b131cffc5ec3ba1c25553";
    };
    deps = {
      "punycode-1.2.4" = self.by-version."punycode"."1.2.4";
      "mimelib-0.2.19" = self.by-version."mimelib"."0.2.19";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."dom-serializer"."0" =
    self.by-version."dom-serializer"."0.1.0";
  by-version."dom-serializer"."0.1.0" = self.buildNodePackage {
    name = "dom-serializer-0.1.0";
    version = "0.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/dom-serializer/-/dom-serializer-0.1.0.tgz";
      name = "dom-serializer-0.1.0.tgz";
      sha1 = "073c697546ce0780ce23be4a28e293e40bc30c82";
    };
    deps = {
      "domelementtype-1.1.3" = self.by-version."domelementtype"."1.1.3";
      "entities-1.1.1" = self.by-version."entities"."1.1.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."dom-serializer"."~0.0.0" =
    self.by-version."dom-serializer"."0.0.1";
  by-version."dom-serializer"."0.0.1" = self.buildNodePackage {
    name = "dom-serializer-0.0.1";
    version = "0.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/dom-serializer/-/dom-serializer-0.0.1.tgz";
      name = "dom-serializer-0.0.1.tgz";
      sha1 = "9589827f1e32d22c37c829adabd59b3247af8eaf";
    };
    deps = {
      "domelementtype-1.1.3" = self.by-version."domelementtype"."1.1.3";
      "entities-1.1.1" = self.by-version."entities"."1.1.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."dom-serializer"."~0.1.0" =
    self.by-version."dom-serializer"."0.1.0";
  by-spec."domelementtype"."1" =
    self.by-version."domelementtype"."1.3.0";
  by-version."domelementtype"."1.3.0" = self.buildNodePackage {
    name = "domelementtype-1.3.0";
    version = "1.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/domelementtype/-/domelementtype-1.3.0.tgz";
      name = "domelementtype-1.3.0.tgz";
      sha1 = "b17aed82e8ab59e52dd9c19b1756e0fc187204c2";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."domelementtype"."~1.1.1" =
    self.by-version."domelementtype"."1.1.3";
  by-version."domelementtype"."1.1.3" = self.buildNodePackage {
    name = "domelementtype-1.1.3";
    version = "1.1.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/domelementtype/-/domelementtype-1.1.3.tgz";
      name = "domelementtype-1.1.3.tgz";
      sha1 = "bd28773e2642881aec51544924299c5cd822185b";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."domhandler"."2.2" =
    self.by-version."domhandler"."2.2.1";
  by-version."domhandler"."2.2.1" = self.buildNodePackage {
    name = "domhandler-2.2.1";
    version = "2.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/domhandler/-/domhandler-2.2.1.tgz";
      name = "domhandler-2.2.1.tgz";
      sha1 = "59df9dcd227e808b365ae73e1f6684ac3d946fc2";
    };
    deps = {
      "domelementtype-1.3.0" = self.by-version."domelementtype"."1.3.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."domhandler"."2.3" =
    self.by-version."domhandler"."2.3.0";
  by-version."domhandler"."2.3.0" = self.buildNodePackage {
    name = "domhandler-2.3.0";
    version = "2.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/domhandler/-/domhandler-2.3.0.tgz";
      name = "domhandler-2.3.0.tgz";
      sha1 = "2de59a0822d5027fabff6f032c2b25a2a8abe738";
    };
    deps = {
      "domelementtype-1.3.0" = self.by-version."domelementtype"."1.3.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."domutils"."1.3" =
    self.by-version."domutils"."1.3.0";
  by-version."domutils"."1.3.0" = self.buildNodePackage {
    name = "domutils-1.3.0";
    version = "1.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/domutils/-/domutils-1.3.0.tgz";
      name = "domutils-1.3.0.tgz";
      sha1 = "9ad4d59b5af6ca684c62fe6d768ef170e70df192";
    };
    deps = {
      "domelementtype-1.3.0" = self.by-version."domelementtype"."1.3.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."domutils"."1.4" =
    self.by-version."domutils"."1.4.3";
  by-version."domutils"."1.4.3" = self.buildNodePackage {
    name = "domutils-1.4.3";
    version = "1.4.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/domutils/-/domutils-1.4.3.tgz";
      name = "domutils-1.4.3.tgz";
      sha1 = "0865513796c6b306031850e175516baf80b72a6f";
    };
    deps = {
      "domelementtype-1.3.0" = self.by-version."domelementtype"."1.3.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."domutils"."1.5" =
    self.by-version."domutils"."1.5.1";
  by-version."domutils"."1.5.1" = self.buildNodePackage {
    name = "domutils-1.5.1";
    version = "1.5.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/domutils/-/domutils-1.5.1.tgz";
      name = "domutils-1.5.1.tgz";
      sha1 = "dcd8488a26f563d61079e48c9f7b7e32373682cf";
    };
    deps = {
      "dom-serializer-0.1.0" = self.by-version."dom-serializer"."0.1.0";
      "domelementtype-1.3.0" = self.by-version."domelementtype"."1.3.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."domutils"."1.5.1" =
    self.by-version."domutils"."1.5.1";
  by-spec."dot-case"."^1.1.0" =
    self.by-version."dot-case"."1.1.2";
  by-version."dot-case"."1.1.2" = self.buildNodePackage {
    name = "dot-case-1.1.2";
    version = "1.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/dot-case/-/dot-case-1.1.2.tgz";
      name = "dot-case-1.1.2.tgz";
      sha1 = "1e73826900de28d6de5480bc1de31d0842b06bec";
    };
    deps = {
      "sentence-case-1.1.3" = self.by-version."sentence-case"."1.1.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."download"."^4.0.0" =
    self.by-version."download"."4.4.3";
  by-version."download"."4.4.3" = self.buildNodePackage {
    name = "download-4.4.3";
    version = "4.4.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/download/-/download-4.4.3.tgz";
      name = "download-4.4.3.tgz";
      sha1 = "aa55fdad392d95d4b68e8c2be03e0c2aa21ba9ac";
    };
    deps = {
      "caw-1.2.0" = self.by-version."caw"."1.2.0";
      "concat-stream-1.5.1" = self.by-version."concat-stream"."1.5.1";
      "each-async-1.1.1" = self.by-version."each-async"."1.1.1";
      "filenamify-1.2.1" = self.by-version."filenamify"."1.2.1";
      "got-5.6.0" = self.by-version."got"."5.6.0";
      "gulp-decompress-1.2.0" = self.by-version."gulp-decompress"."1.2.0";
      "gulp-rename-1.2.2" = self.by-version."gulp-rename"."1.2.2";
      "is-url-1.2.2" = self.by-version."is-url"."1.2.2";
      "object-assign-4.1.0" = self.by-version."object-assign"."4.1.0";
      "read-all-stream-3.1.0" = self.by-version."read-all-stream"."3.1.0";
      "readable-stream-2.1.5" = self.by-version."readable-stream"."2.1.5";
      "stream-combiner2-1.1.1" = self.by-version."stream-combiner2"."1.1.1";
      "vinyl-1.2.0" = self.by-version."vinyl"."1.2.0";
      "vinyl-fs-2.4.3" = self.by-version."vinyl-fs"."2.4.3";
      "ware-1.3.0" = self.by-version."ware"."1.3.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."download"."^4.1.2" =
    self.by-version."download"."4.4.3";
  by-spec."duplexer"."~0.1.1" =
    self.by-version."duplexer"."0.1.1";
  by-version."duplexer"."0.1.1" = self.buildNodePackage {
    name = "duplexer-0.1.1";
    version = "0.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/duplexer/-/duplexer-0.1.1.tgz";
      name = "duplexer-0.1.1.tgz";
      sha1 = "ace6ff808c1ce66b57d1ebf97977acb02334cfc1";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."duplexer2"."0.0.2" =
    self.by-version."duplexer2"."0.0.2";
  by-version."duplexer2"."0.0.2" = self.buildNodePackage {
    name = "duplexer2-0.0.2";
    version = "0.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/duplexer2/-/duplexer2-0.0.2.tgz";
      name = "duplexer2-0.0.2.tgz";
      sha1 = "c614dcf67e2fb14995a91711e5a617e8a60a31db";
    };
    deps = {
      "readable-stream-1.1.14" = self.by-version."readable-stream"."1.1.14";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."duplexer2"."^0.1.4" =
    self.by-version."duplexer2"."0.1.4";
  by-version."duplexer2"."0.1.4" = self.buildNodePackage {
    name = "duplexer2-0.1.4";
    version = "0.1.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/duplexer2/-/duplexer2-0.1.4.tgz";
      name = "duplexer2-0.1.4.tgz";
      sha1 = "8b12dab878c0d69e3e7891051662a32fc6bddcc1";
    };
    deps = {
      "readable-stream-2.1.5" = self.by-version."readable-stream"."2.1.5";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."duplexer2"."~0.1.0" =
    self.by-version."duplexer2"."0.1.4";
  by-spec."duplexify"."^3.1.2" =
    self.by-version."duplexify"."3.4.5";
  by-version."duplexify"."3.4.5" = self.buildNodePackage {
    name = "duplexify-3.4.5";
    version = "3.4.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/duplexify/-/duplexify-3.4.5.tgz";
      name = "duplexify-3.4.5.tgz";
      sha1 = "0e7e287a775af753bf57e6e7b7f21f183f6c3a53";
    };
    deps = {
      "end-of-stream-1.0.0" = self.by-version."end-of-stream"."1.0.0";
      "inherits-2.0.1" = self.by-version."inherits"."2.0.1";
      "readable-stream-2.1.5" = self.by-version."readable-stream"."2.1.5";
      "stream-shift-1.0.0" = self.by-version."stream-shift"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."duplexify"."^3.2.0" =
    self.by-version."duplexify"."3.4.5";
  by-spec."duplexify"."^3.4.2" =
    self.by-version."duplexify"."3.4.5";
  by-spec."each-async"."^1.0.0" =
    self.by-version."each-async"."1.1.1";
  by-version."each-async"."1.1.1" = self.buildNodePackage {
    name = "each-async-1.1.1";
    version = "1.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/each-async/-/each-async-1.1.1.tgz";
      name = "each-async-1.1.1.tgz";
      sha1 = "dee5229bdf0ab6ba2012a395e1b869abf8813473";
    };
    deps = {
      "onetime-1.1.0" = self.by-version."onetime"."1.1.0";
      "set-immediate-shim-1.0.1" = self.by-version."set-immediate-shim"."1.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."each-async"."^1.1.1" =
    self.by-version."each-async"."1.1.1";
  by-spec."easy-extender"."^2.3.1" =
    self.by-version."easy-extender"."2.3.2";
  by-version."easy-extender"."2.3.2" = self.buildNodePackage {
    name = "easy-extender-2.3.2";
    version = "2.3.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/easy-extender/-/easy-extender-2.3.2.tgz";
      name = "easy-extender-2.3.2.tgz";
      sha1 = "3d3248febe2b159607316d8f9cf491c16648221d";
    };
    deps = {
      "lodash-3.10.1" = self.by-version."lodash"."3.10.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."eazy-logger"."^2.0.0" =
    self.by-version."eazy-logger"."2.1.3";
  by-version."eazy-logger"."2.1.3" = self.buildNodePackage {
    name = "eazy-logger-2.1.3";
    version = "2.1.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/eazy-logger/-/eazy-logger-2.1.3.tgz";
      name = "eazy-logger-2.1.3.tgz";
      sha1 = "eeca32b552e6ec926a19b60366dfff3894300675";
    };
    deps = {
      "lodash.clonedeep-4.3.1" = self.by-version."lodash.clonedeep"."4.3.1";
      "opt-merger-1.1.1" = self.by-version."opt-merger"."1.1.1";
      "tfunk-3.0.2" = self.by-version."tfunk"."3.0.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."eazy-logger"."^2.1.2" =
    self.by-version."eazy-logger"."2.1.3";
  by-spec."ecc-jsbn"."~0.1.1" =
    self.by-version."ecc-jsbn"."0.1.1";
  by-version."ecc-jsbn"."0.1.1" = self.buildNodePackage {
    name = "ecc-jsbn-0.1.1";
    version = "0.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/ecc-jsbn/-/ecc-jsbn-0.1.1.tgz";
      name = "ecc-jsbn-0.1.1.tgz";
      sha1 = "0fc73a9ed5f0d53c38193398523ef7e543777505";
    };
    deps = {
      "jsbn-0.1.0" = self.by-version."jsbn"."0.1.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."ee-first"."1.1.1" =
    self.by-version."ee-first"."1.1.1";
  by-version."ee-first"."1.1.1" = self.buildNodePackage {
    name = "ee-first-1.1.1";
    version = "1.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/ee-first/-/ee-first-1.1.1.tgz";
      name = "ee-first-1.1.1.tgz";
      sha1 = "590c61156b0ae2f4f0255732a158b266bc56b21d";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."emitter-steward"."^1.0.0" =
    self.by-version."emitter-steward"."1.0.0";
  by-version."emitter-steward"."1.0.0" = self.buildNodePackage {
    name = "emitter-steward-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/emitter-steward/-/emitter-steward-1.0.0.tgz";
      name = "emitter-steward-1.0.0.tgz";
      sha1 = "f3411ade9758a7565df848b2da0cbbd1b46cbd64";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."encodeurl"."~1.0.1" =
    self.by-version."encodeurl"."1.0.1";
  by-version."encodeurl"."1.0.1" = self.buildNodePackage {
    name = "encodeurl-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/encodeurl/-/encodeurl-1.0.1.tgz";
      name = "encodeurl-1.0.1.tgz";
      sha1 = "79e3d58655346909fe6f0f45a5de68103b294d20";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."encoding"."~0.1.7" =
    self.by-version."encoding"."0.1.12";
  by-version."encoding"."0.1.12" = self.buildNodePackage {
    name = "encoding-0.1.12";
    version = "0.1.12";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/encoding/-/encoding-0.1.12.tgz";
      name = "encoding-0.1.12.tgz";
      sha1 = "538b66f3ee62cd1ab51ec323829d1f9480c74beb";
    };
    deps = {
      "iconv-lite-0.4.13" = self.by-version."iconv-lite"."0.4.13";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."end-of-stream"."1.0.0" =
    self.by-version."end-of-stream"."1.0.0";
  by-version."end-of-stream"."1.0.0" = self.buildNodePackage {
    name = "end-of-stream-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/end-of-stream/-/end-of-stream-1.0.0.tgz";
      name = "end-of-stream-1.0.0.tgz";
      sha1 = "d4596e702734a93e40e9af864319eabd99ff2f0e";
    };
    deps = {
      "once-1.3.3" = self.by-version."once"."1.3.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."end-of-stream"."^1.0.0" =
    self.by-version."end-of-stream"."1.1.0";
  by-version."end-of-stream"."1.1.0" = self.buildNodePackage {
    name = "end-of-stream-1.1.0";
    version = "1.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/end-of-stream/-/end-of-stream-1.1.0.tgz";
      name = "end-of-stream-1.1.0.tgz";
      sha1 = "e9353258baa9108965efc41cb0ef8ade2f3cfb07";
    };
    deps = {
      "once-1.3.3" = self.by-version."once"."1.3.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."end-of-stream"."^1.1.0" =
    self.by-version."end-of-stream"."1.1.0";
  by-spec."end-of-stream"."~0.1.5" =
    self.by-version."end-of-stream"."0.1.5";
  by-version."end-of-stream"."0.1.5" = self.buildNodePackage {
    name = "end-of-stream-0.1.5";
    version = "0.1.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/end-of-stream/-/end-of-stream-0.1.5.tgz";
      name = "end-of-stream-0.1.5.tgz";
      sha1 = "8e177206c3c80837d85632e8b9359dfe8b2f6eaf";
    };
    deps = {
      "once-1.3.3" = self.by-version."once"."1.3.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."engine.io"."1.6.8" =
    self.by-version."engine.io"."1.6.8";
  by-version."engine.io"."1.6.8" = self.buildNodePackage {
    name = "engine.io-1.6.8";
    version = "1.6.8";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/engine.io/-/engine.io-1.6.8.tgz";
      name = "engine.io-1.6.8.tgz";
      sha1 = "de05a06b757e7517695e088c7b051c47819f511b";
    };
    deps = {
      "base64id-0.1.0" = self.by-version."base64id"."0.1.0";
      "debug-2.2.0" = self.by-version."debug"."2.2.0";
      "ws-1.0.1" = self.by-version."ws"."1.0.1";
      "engine.io-parser-1.2.4" = self.by-version."engine.io-parser"."1.2.4";
      "accepts-1.1.4" = self.by-version."accepts"."1.1.4";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."engine.io-client"."1.6.8" =
    self.by-version."engine.io-client"."1.6.8";
  by-version."engine.io-client"."1.6.8" = self.buildNodePackage {
    name = "engine.io-client-1.6.8";
    version = "1.6.8";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/engine.io-client/-/engine.io-client-1.6.8.tgz";
      name = "engine.io-client-1.6.8.tgz";
      sha1 = "6e2db11648b45e405c46b172ea3e3dac37cc0ceb";
    };
    deps = {
      "has-cors-1.1.0" = self.by-version."has-cors"."1.1.0";
      "ws-1.0.1" = self.by-version."ws"."1.0.1";
      "xmlhttprequest-ssl-1.5.1" = self.by-version."xmlhttprequest-ssl"."1.5.1";
      "component-emitter-1.1.2" = self.by-version."component-emitter"."1.1.2";
      "indexof-0.0.1" = self.by-version."indexof"."0.0.1";
      "engine.io-parser-1.2.4" = self.by-version."engine.io-parser"."1.2.4";
      "debug-2.2.0" = self.by-version."debug"."2.2.0";
      "parseuri-0.0.4" = self.by-version."parseuri"."0.0.4";
      "parsejson-0.0.1" = self.by-version."parsejson"."0.0.1";
      "parseqs-0.0.2" = self.by-version."parseqs"."0.0.2";
      "component-inherit-0.0.3" = self.by-version."component-inherit"."0.0.3";
      "yeast-0.1.2" = self.by-version."yeast"."0.1.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."engine.io-parser"."1.2.4" =
    self.by-version."engine.io-parser"."1.2.4";
  by-version."engine.io-parser"."1.2.4" = self.buildNodePackage {
    name = "engine.io-parser-1.2.4";
    version = "1.2.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/engine.io-parser/-/engine.io-parser-1.2.4.tgz";
      name = "engine.io-parser-1.2.4.tgz";
      sha1 = "e0897b0bf14e792d4cd2a5950553919c56948c42";
    };
    deps = {
      "after-0.8.1" = self.by-version."after"."0.8.1";
      "arraybuffer.slice-0.0.6" = self.by-version."arraybuffer.slice"."0.0.6";
      "base64-arraybuffer-0.1.2" = self.by-version."base64-arraybuffer"."0.1.2";
      "blob-0.0.4" = self.by-version."blob"."0.0.4";
      "has-binary-0.1.6" = self.by-version."has-binary"."0.1.6";
      "utf8-2.1.0" = self.by-version."utf8"."2.1.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."entities"."0.x" =
    self.by-version."entities"."0.5.0";
  by-version."entities"."0.5.0" = self.buildNodePackage {
    name = "entities-0.5.0";
    version = "0.5.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/entities/-/entities-0.5.0.tgz";
      name = "entities-0.5.0.tgz";
      sha1 = "f611cb5ae221050e0012c66979503fd7ae19cc49";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."entities"."1.0" =
    self.by-version."entities"."1.0.0";
  by-version."entities"."1.0.0" = self.buildNodePackage {
    name = "entities-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/entities/-/entities-1.0.0.tgz";
      name = "entities-1.0.0.tgz";
      sha1 = "b2987aa3821347fcde642b24fdfc9e4fb712bf26";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."entities"."~1.1.1" =
    self.by-version."entities"."1.1.1";
  by-version."entities"."1.1.1" = self.buildNodePackage {
    name = "entities-1.1.1";
    version = "1.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/entities/-/entities-1.1.1.tgz";
      name = "entities-1.1.1.tgz";
      sha1 = "6e5c2d0a5621b5dadaecef80b90edfb5cd7772f0";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."error-ex"."^1.2.0" =
    self.by-version."error-ex"."1.3.0";
  by-version."error-ex"."1.3.0" = self.buildNodePackage {
    name = "error-ex-1.3.0";
    version = "1.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/error-ex/-/error-ex-1.3.0.tgz";
      name = "error-ex-1.3.0.tgz";
      sha1 = "e67b43f3e82c96ea3a584ffee0b9fc3325d802d9";
    };
    deps = {
      "is-arrayish-0.2.1" = self.by-version."is-arrayish"."0.2.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."es5-ext"."^0.10.7" =
    self.by-version."es5-ext"."0.10.12";
  by-version."es5-ext"."0.10.12" = self.buildNodePackage {
    name = "es5-ext-0.10.12";
    version = "0.10.12";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/es5-ext/-/es5-ext-0.10.12.tgz";
      name = "es5-ext-0.10.12.tgz";
      sha1 = "aa84641d4db76b62abba5e45fd805ecbab140047";
    };
    deps = {
      "es6-iterator-2.0.0" = self.by-version."es6-iterator"."2.0.0";
      "es6-symbol-3.1.0" = self.by-version."es6-symbol"."3.1.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."es5-ext"."~0.10.11" =
    self.by-version."es5-ext"."0.10.12";
  by-spec."es5-ext"."~0.10.2" =
    self.by-version."es5-ext"."0.10.12";
  by-spec."es6-iterator"."2" =
    self.by-version."es6-iterator"."2.0.0";
  by-version."es6-iterator"."2.0.0" = self.buildNodePackage {
    name = "es6-iterator-2.0.0";
    version = "2.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/es6-iterator/-/es6-iterator-2.0.0.tgz";
      name = "es6-iterator-2.0.0.tgz";
      sha1 = "bd968567d61635e33c0b80727613c9cb4b096bac";
    };
    deps = {
      "d-0.1.1" = self.by-version."d"."0.1.1";
      "es5-ext-0.10.12" = self.by-version."es5-ext"."0.10.12";
      "es6-symbol-3.1.0" = self.by-version."es6-symbol"."3.1.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."es6-symbol"."3" =
    self.by-version."es6-symbol"."3.1.0";
  by-version."es6-symbol"."3.1.0" = self.buildNodePackage {
    name = "es6-symbol-3.1.0";
    version = "3.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/es6-symbol/-/es6-symbol-3.1.0.tgz";
      name = "es6-symbol-3.1.0.tgz";
      sha1 = "94481c655e7a7cad82eba832d97d5433496d7ffa";
    };
    deps = {
      "d-0.1.1" = self.by-version."d"."0.1.1";
      "es5-ext-0.10.12" = self.by-version."es5-ext"."0.10.12";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."es6-symbol"."^3.0.2" =
    self.by-version."es6-symbol"."3.1.0";
  by-spec."es6-symbol"."~3.1" =
    self.by-version."es6-symbol"."3.1.0";
  by-spec."escape-html"."~1.0.3" =
    self.by-version."escape-html"."1.0.3";
  by-version."escape-html"."1.0.3" = self.buildNodePackage {
    name = "escape-html-1.0.3";
    version = "1.0.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/escape-html/-/escape-html-1.0.3.tgz";
      name = "escape-html-1.0.3.tgz";
      sha1 = "0258eae4d3d0c0974de1c169188ef0051d1d1988";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."escape-string-regexp"."^1.0.0" =
    self.by-version."escape-string-regexp"."1.0.5";
  by-version."escape-string-regexp"."1.0.5" = self.buildNodePackage {
    name = "escape-string-regexp-1.0.5";
    version = "1.0.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/escape-string-regexp/-/escape-string-regexp-1.0.5.tgz";
      name = "escape-string-regexp-1.0.5.tgz";
      sha1 = "1b61c0562190a8dff6ae3bb2cf0200ca130b86d4";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."escape-string-regexp"."^1.0.2" =
    self.by-version."escape-string-regexp"."1.0.5";
  by-spec."escape-string-regexp"."^1.0.3" =
    self.by-version."escape-string-regexp"."1.0.5";
  by-spec."escape-string-regexp"."^1.0.5" =
    self.by-version."escape-string-regexp"."1.0.5";
  by-spec."escodegen"."^1.6.1" =
    self.by-version."escodegen"."1.8.1";
  by-version."escodegen"."1.8.1" = self.buildNodePackage {
    name = "escodegen-1.8.1";
    version = "1.8.1";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/escodegen/-/escodegen-1.8.1.tgz";
      name = "escodegen-1.8.1.tgz";
      sha1 = "5a5b53af4693110bebb0867aa3430dd3b70a1018";
    };
    deps = {
      "estraverse-1.9.3" = self.by-version."estraverse"."1.9.3";
      "esutils-2.0.2" = self.by-version."esutils"."2.0.2";
      "esprima-2.7.2" = self.by-version."esprima"."2.7.2";
      "optionator-0.8.1" = self.by-version."optionator"."0.8.1";
    };
    optionalDependencies = {
      "source-map-0.2.0" = self.by-version."source-map"."0.2.0";
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."esprima"."^2.6.0" =
    self.by-version."esprima"."2.7.2";
  by-version."esprima"."2.7.2" = self.buildNodePackage {
    name = "esprima-2.7.2";
    version = "2.7.2";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/esprima/-/esprima-2.7.2.tgz";
      name = "esprima-2.7.2.tgz";
      sha1 = "f43be543609984eae44c933ac63352a6af35f339";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."esprima"."^2.7.1" =
    self.by-version."esprima"."2.7.2";
  by-spec."estraverse"."^1.9.1" =
    self.by-version."estraverse"."1.9.3";
  by-version."estraverse"."1.9.3" = self.buildNodePackage {
    name = "estraverse-1.9.3";
    version = "1.9.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/estraverse/-/estraverse-1.9.3.tgz";
      name = "estraverse-1.9.3.tgz";
      sha1 = "af67f2dc922582415950926091a4005d29c9bb44";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."esutils"."^2.0.2" =
    self.by-version."esutils"."2.0.2";
  by-version."esutils"."2.0.2" = self.buildNodePackage {
    name = "esutils-2.0.2";
    version = "2.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/esutils/-/esutils-2.0.2.tgz";
      name = "esutils-2.0.2.tgz";
      sha1 = "0abf4f1caa5bcb1f7a9d8acc6dea4faaa04bac9b";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."etag"."^1.7.0" =
    self.by-version."etag"."1.7.0";
  by-version."etag"."1.7.0" = self.buildNodePackage {
    name = "etag-1.7.0";
    version = "1.7.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/etag/-/etag-1.7.0.tgz";
      name = "etag-1.7.0.tgz";
      sha1 = "03d30b5f67dd6e632d2945d30d6652731a34d5d8";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."etag"."~1.7.0" =
    self.by-version."etag"."1.7.0";
  by-spec."event-stream"."~3.1.5" =
    self.by-version."event-stream"."3.1.7";
  by-version."event-stream"."3.1.7" = self.buildNodePackage {
    name = "event-stream-3.1.7";
    version = "3.1.7";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/event-stream/-/event-stream-3.1.7.tgz";
      name = "event-stream-3.1.7.tgz";
      sha1 = "b4c540012d0fe1498420f3d8946008db6393c37a";
    };
    deps = {
      "through-2.3.8" = self.by-version."through"."2.3.8";
      "duplexer-0.1.1" = self.by-version."duplexer"."0.1.1";
      "from-0.1.3" = self.by-version."from"."0.1.3";
      "map-stream-0.1.0" = self.by-version."map-stream"."0.1.0";
      "pause-stream-0.0.11" = self.by-version."pause-stream"."0.0.11";
      "split-0.2.10" = self.by-version."split"."0.2.10";
      "stream-combiner-0.0.4" = self.by-version."stream-combiner"."0.0.4";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."eventemitter3"."1.x.x" =
    self.by-version."eventemitter3"."1.2.0";
  by-version."eventemitter3"."1.2.0" = self.buildNodePackage {
    name = "eventemitter3-1.2.0";
    version = "1.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/eventemitter3/-/eventemitter3-1.2.0.tgz";
      name = "eventemitter3-1.2.0.tgz";
      sha1 = "1c86991d816ad1e504750e73874224ecf3bec508";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."exec-buffer"."^2.0.0" =
    self.by-version."exec-buffer"."2.0.1";
  by-version."exec-buffer"."2.0.1" = self.buildNodePackage {
    name = "exec-buffer-2.0.1";
    version = "2.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/exec-buffer/-/exec-buffer-2.0.1.tgz";
      name = "exec-buffer-2.0.1.tgz";
      sha1 = "0028a31be0b1460b61d075f96af4583b9e335ea0";
    };
    deps = {
      "rimraf-2.5.4" = self.by-version."rimraf"."2.5.4";
      "tempfile-1.1.1" = self.by-version."tempfile"."1.1.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."exec-series"."^1.0.0" =
    self.by-version."exec-series"."1.0.3";
  by-version."exec-series"."1.0.3" = self.buildNodePackage {
    name = "exec-series-1.0.3";
    version = "1.0.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/exec-series/-/exec-series-1.0.3.tgz";
      name = "exec-series-1.0.3.tgz";
      sha1 = "6d257a9beac482a872c7783bc8615839fc77143a";
    };
    deps = {
      "async-each-series-1.1.0" = self.by-version."async-each-series"."1.1.0";
      "object-assign-4.1.0" = self.by-version."object-assign"."4.1.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."executable"."^1.0.0" =
    self.by-version."executable"."1.1.0";
  by-version."executable"."1.1.0" = self.buildNodePackage {
    name = "executable-1.1.0";
    version = "1.1.0";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/executable/-/executable-1.1.0.tgz";
      name = "executable-1.1.0.tgz";
      sha1 = "877980e9112f3391066da37265de7ad8434ab4d9";
    };
    deps = {
      "meow-3.7.0" = self.by-version."meow"."3.7.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."exit"."0.1.2" =
    self.by-version."exit"."0.1.2";
  by-version."exit"."0.1.2" = self.buildNodePackage {
    name = "exit-0.1.2";
    version = "0.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/exit/-/exit-0.1.2.tgz";
      name = "exit-0.1.2.tgz";
      sha1 = "0632638f8d877cc82107d30a0fff1a17cba1cd0c";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."expand-brackets"."^0.1.4" =
    self.by-version."expand-brackets"."0.1.5";
  by-version."expand-brackets"."0.1.5" = self.buildNodePackage {
    name = "expand-brackets-0.1.5";
    version = "0.1.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/expand-brackets/-/expand-brackets-0.1.5.tgz";
      name = "expand-brackets-0.1.5.tgz";
      sha1 = "df07284e342a807cd733ac5af72411e581d1177b";
    };
    deps = {
      "is-posix-bracket-0.1.1" = self.by-version."is-posix-bracket"."0.1.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."expand-range"."^1.8.1" =
    self.by-version."expand-range"."1.8.2";
  by-version."expand-range"."1.8.2" = self.buildNodePackage {
    name = "expand-range-1.8.2";
    version = "1.8.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/expand-range/-/expand-range-1.8.2.tgz";
      name = "expand-range-1.8.2.tgz";
      sha1 = "a299effd335fe2721ebae8e257ec79644fc85337";
    };
    deps = {
      "fill-range-2.2.3" = self.by-version."fill-range"."2.2.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."expand-tilde"."^1.2.1" =
    self.by-version."expand-tilde"."1.2.2";
  by-version."expand-tilde"."1.2.2" = self.buildNodePackage {
    name = "expand-tilde-1.2.2";
    version = "1.2.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/expand-tilde/-/expand-tilde-1.2.2.tgz";
      name = "expand-tilde-1.2.2.tgz";
      sha1 = "0b81eba897e5a3d31d1c3d102f8f01441e559449";
    };
    deps = {
      "os-homedir-1.0.1" = self.by-version."os-homedir"."1.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."expand-tilde"."^1.2.2" =
    self.by-version."expand-tilde"."1.2.2";
  by-spec."express"."2.5.x" =
    self.by-version."express"."2.5.11";
  by-version."express"."2.5.11" = self.buildNodePackage {
    name = "express-2.5.11";
    version = "2.5.11";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/express/-/express-2.5.11.tgz";
      name = "express-2.5.11.tgz";
      sha1 = "4ce8ea1f3635e69e49f0ebb497b6a4b0a51ce6f0";
    };
    deps = {
      "connect-1.9.2" = self.by-version."connect"."1.9.2";
      "mime-1.2.4" = self.by-version."mime"."1.2.4";
      "qs-0.4.2" = self.by-version."qs"."0.4.2";
      "mkdirp-0.3.0" = self.by-version."mkdirp"."0.3.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."extend"."3.0.0" =
    self.by-version."extend"."3.0.0";
  by-version."extend"."3.0.0" = self.buildNodePackage {
    name = "extend-3.0.0";
    version = "3.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/extend/-/extend-3.0.0.tgz";
      name = "extend-3.0.0.tgz";
      sha1 = "5a474353b9f3353ddd8176dfd37b91c83a46f1d4";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."extend"."^1.3.0" =
    self.by-version."extend"."1.3.0";
  by-version."extend"."1.3.0" = self.buildNodePackage {
    name = "extend-1.3.0";
    version = "1.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/extend/-/extend-1.3.0.tgz";
      name = "extend-1.3.0.tgz";
      sha1 = "d1516fb0ff5624d2ebf9123ea1dac5a1994004f8";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."extend"."^3.0.0" =
    self.by-version."extend"."3.0.0";
  by-spec."extend"."~1.2.1" =
    self.by-version."extend"."1.2.1";
  by-version."extend"."1.2.1" = self.buildNodePackage {
    name = "extend-1.2.1";
    version = "1.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/extend/-/extend-1.2.1.tgz";
      name = "extend-1.2.1.tgz";
      sha1 = "a0f5fd6cfc83a5fe49ef698d60ec8a624dd4576c";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."extend"."~3.0.0" =
    self.by-version."extend"."3.0.0";
  by-spec."extend-shallow"."^2.0.1" =
    self.by-version."extend-shallow"."2.0.1";
  by-version."extend-shallow"."2.0.1" = self.buildNodePackage {
    name = "extend-shallow-2.0.1";
    version = "2.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/extend-shallow/-/extend-shallow-2.0.1.tgz";
      name = "extend-shallow-2.0.1.tgz";
      sha1 = "51af7d614ad9a9f610ea1bafbb989d6b1c56890f";
    };
    deps = {
      "is-extendable-0.1.1" = self.by-version."is-extendable"."0.1.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."extglob"."^0.3.1" =
    self.by-version."extglob"."0.3.2";
  by-version."extglob"."0.3.2" = self.buildNodePackage {
    name = "extglob-0.3.2";
    version = "0.3.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/extglob/-/extglob-0.3.2.tgz";
      name = "extglob-0.3.2.tgz";
      sha1 = "2e18ff3d2f49ab2765cec9023f011daa8d8349a1";
    };
    deps = {
      "is-extglob-1.0.0" = self.by-version."is-extglob"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."extract-css"."^1.0.0" =
    self.by-version."extract-css"."1.0.3";
  by-version."extract-css"."1.0.3" = self.buildNodePackage {
    name = "extract-css-1.0.3";
    version = "1.0.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/extract-css/-/extract-css-1.0.3.tgz";
      name = "extract-css-1.0.3.tgz";
      sha1 = "aca7b84bf7e138a5dfab2b9024a499549f935046";
    };
    deps = {
      "batch-0.5.3" = self.by-version."batch"."0.5.3";
      "href-content-1.0.2" = self.by-version."href-content"."1.0.2";
      "list-stylesheets-1.1.0" = self.by-version."list-stylesheets"."1.1.0";
      "style-data-1.1.2" = self.by-version."style-data"."1.1.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."extsprintf"."1.0.2" =
    self.by-version."extsprintf"."1.0.2";
  by-version."extsprintf"."1.0.2" = self.buildNodePackage {
    name = "extsprintf-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/extsprintf/-/extsprintf-1.0.2.tgz";
      name = "extsprintf-1.0.2.tgz";
      sha1 = "e1080e0658e300b06294990cc70e1502235fd550";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."fancy-log"."^1.1.0" =
    self.by-version."fancy-log"."1.2.0";
  by-version."fancy-log"."1.2.0" = self.buildNodePackage {
    name = "fancy-log-1.2.0";
    version = "1.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/fancy-log/-/fancy-log-1.2.0.tgz";
      name = "fancy-log-1.2.0.tgz";
      sha1 = "d5a51b53e9ab22ca07d558f2b67ae55fdb5fcbd8";
    };
    deps = {
      "chalk-1.1.3" = self.by-version."chalk"."1.1.3";
      "time-stamp-1.0.1" = self.by-version."time-stamp"."1.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."fast-levenshtein"."^1.1.0" =
    self.by-version."fast-levenshtein"."1.1.4";
  by-version."fast-levenshtein"."1.1.4" = self.buildNodePackage {
    name = "fast-levenshtein-1.1.4";
    version = "1.1.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/fast-levenshtein/-/fast-levenshtein-1.1.4.tgz";
      name = "fast-levenshtein-1.1.4.tgz";
      sha1 = "e6a754cc8f15e58987aa9cbd27af66fd6f4e5af9";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."fd-slicer"."~1.0.1" =
    self.by-version."fd-slicer"."1.0.1";
  by-version."fd-slicer"."1.0.1" = self.buildNodePackage {
    name = "fd-slicer-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/fd-slicer/-/fd-slicer-1.0.1.tgz";
      name = "fd-slicer-1.0.1.tgz";
      sha1 = "8b5bcbd9ec327c5041bf9ab023fd6750f1177e65";
    };
    deps = {
      "pend-1.2.0" = self.by-version."pend"."1.2.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."figures"."^1.3.5" =
    self.by-version."figures"."1.7.0";
  by-version."figures"."1.7.0" = self.buildNodePackage {
    name = "figures-1.7.0";
    version = "1.7.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/figures/-/figures-1.7.0.tgz";
      name = "figures-1.7.0.tgz";
      sha1 = "cbe1e3affcf1cd44b80cadfed28dc793a9701d2e";
    };
    deps = {
      "escape-string-regexp-1.0.5" = self.by-version."escape-string-regexp"."1.0.5";
      "object-assign-4.1.0" = self.by-version."object-assign"."4.1.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."file-type"."^3.1.0" =
    self.by-version."file-type"."3.8.0";
  by-version."file-type"."3.8.0" = self.buildNodePackage {
    name = "file-type-3.8.0";
    version = "3.8.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/file-type/-/file-type-3.8.0.tgz";
      name = "file-type-3.8.0.tgz";
      sha1 = "bcadf6a8f624ebe4a10e5ad26727b6b93f16d78d";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."filename-regex"."^2.0.0" =
    self.by-version."filename-regex"."2.0.0";
  by-version."filename-regex"."2.0.0" = self.buildNodePackage {
    name = "filename-regex-2.0.0";
    version = "2.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/filename-regex/-/filename-regex-2.0.0.tgz";
      name = "filename-regex-2.0.0.tgz";
      sha1 = "996e3e80479b98b9897f15a8a58b3d084e926775";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."filename-reserved-regex"."^1.0.0" =
    self.by-version."filename-reserved-regex"."1.0.0";
  by-version."filename-reserved-regex"."1.0.0" = self.buildNodePackage {
    name = "filename-reserved-regex-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/filename-reserved-regex/-/filename-reserved-regex-1.0.0.tgz";
      name = "filename-reserved-regex-1.0.0.tgz";
      sha1 = "e61cf805f0de1c984567d0386dc5df50ee5af7e4";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."filenamify"."^1.0.1" =
    self.by-version."filenamify"."1.2.1";
  by-version."filenamify"."1.2.1" = self.buildNodePackage {
    name = "filenamify-1.2.1";
    version = "1.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/filenamify/-/filenamify-1.2.1.tgz";
      name = "filenamify-1.2.1.tgz";
      sha1 = "a9f2ffd11c503bed300015029272378f1f1365a5";
    };
    deps = {
      "filename-reserved-regex-1.0.0" = self.by-version."filename-reserved-regex"."1.0.0";
      "strip-outer-1.0.0" = self.by-version."strip-outer"."1.0.0";
      "trim-repeated-1.0.0" = self.by-version."trim-repeated"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."fill-range"."^2.1.0" =
    self.by-version."fill-range"."2.2.3";
  by-version."fill-range"."2.2.3" = self.buildNodePackage {
    name = "fill-range-2.2.3";
    version = "2.2.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/fill-range/-/fill-range-2.2.3.tgz";
      name = "fill-range-2.2.3.tgz";
      sha1 = "50b77dfd7e469bc7492470963699fe7a8485a723";
    };
    deps = {
      "is-number-2.1.0" = self.by-version."is-number"."2.1.0";
      "isobject-2.1.0" = self.by-version."isobject"."2.1.0";
      "randomatic-1.1.5" = self.by-version."randomatic"."1.1.5";
      "repeat-element-1.1.2" = self.by-version."repeat-element"."1.1.2";
      "repeat-string-1.5.4" = self.by-version."repeat-string"."1.5.4";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."finalhandler"."0.4.1" =
    self.by-version."finalhandler"."0.4.1";
  by-version."finalhandler"."0.4.1" = self.buildNodePackage {
    name = "finalhandler-0.4.1";
    version = "0.4.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/finalhandler/-/finalhandler-0.4.1.tgz";
      name = "finalhandler-0.4.1.tgz";
      sha1 = "85a17c6c59a94717d262d61230d4b0ebe3d4a14d";
    };
    deps = {
      "debug-2.2.0" = self.by-version."debug"."2.2.0";
      "escape-html-1.0.3" = self.by-version."escape-html"."1.0.3";
      "on-finished-2.3.0" = self.by-version."on-finished"."2.3.0";
      "unpipe-1.0.0" = self.by-version."unpipe"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."find-index"."^0.1.1" =
    self.by-version."find-index"."0.1.1";
  by-version."find-index"."0.1.1" = self.buildNodePackage {
    name = "find-index-0.1.1";
    version = "0.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/find-index/-/find-index-0.1.1.tgz";
      name = "find-index-0.1.1.tgz";
      sha1 = "675d358b2ca3892d795a1ab47232f8b6e2e0dde4";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."find-up"."^1.0.0" =
    self.by-version."find-up"."1.1.2";
  by-version."find-up"."1.1.2" = self.buildNodePackage {
    name = "find-up-1.1.2";
    version = "1.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/find-up/-/find-up-1.1.2.tgz";
      name = "find-up-1.1.2.tgz";
      sha1 = "6b2e9822b1a2ce0a60ab64d610eccad53cb24d0f";
    };
    deps = {
      "path-exists-2.1.0" = self.by-version."path-exists"."2.1.0";
      "pinkie-promise-2.0.1" = self.by-version."pinkie-promise"."2.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."find-versions"."^1.0.0" =
    self.by-version."find-versions"."1.2.1";
  by-version."find-versions"."1.2.1" = self.buildNodePackage {
    name = "find-versions-1.2.1";
    version = "1.2.1";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/find-versions/-/find-versions-1.2.1.tgz";
      name = "find-versions-1.2.1.tgz";
      sha1 = "cbde9f12e38575a0af1be1b9a2c5d5fd8f186b62";
    };
    deps = {
      "array-uniq-1.0.3" = self.by-version."array-uniq"."1.0.3";
      "get-stdin-4.0.1" = self.by-version."get-stdin"."4.0.1";
      "meow-3.7.0" = self.by-version."meow"."3.7.0";
      "semver-regex-1.0.0" = self.by-version."semver-regex"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."findup-sync"."^0.2.1" =
    self.by-version."findup-sync"."0.2.1";
  by-version."findup-sync"."0.2.1" = self.buildNodePackage {
    name = "findup-sync-0.2.1";
    version = "0.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/findup-sync/-/findup-sync-0.2.1.tgz";
      name = "findup-sync-0.2.1.tgz";
      sha1 = "e0a90a450075c49466ee513732057514b81e878c";
    };
    deps = {
      "glob-4.3.5" = self.by-version."glob"."4.3.5";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."findup-sync"."^0.4.2" =
    self.by-version."findup-sync"."0.4.2";
  by-version."findup-sync"."0.4.2" = self.buildNodePackage {
    name = "findup-sync-0.4.2";
    version = "0.4.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/findup-sync/-/findup-sync-0.4.2.tgz";
      name = "findup-sync-0.4.2.tgz";
      sha1 = "a8117d0f73124f5a4546839579fe52d7129fb5e5";
    };
    deps = {
      "detect-file-0.1.0" = self.by-version."detect-file"."0.1.0";
      "is-glob-2.0.1" = self.by-version."is-glob"."2.0.1";
      "micromatch-2.3.11" = self.by-version."micromatch"."2.3.11";
      "resolve-dir-0.1.1" = self.by-version."resolve-dir"."0.1.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."findup-sync"."~0.3.0" =
    self.by-version."findup-sync"."0.3.0";
  by-version."findup-sync"."0.3.0" = self.buildNodePackage {
    name = "findup-sync-0.3.0";
    version = "0.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/findup-sync/-/findup-sync-0.3.0.tgz";
      name = "findup-sync-0.3.0.tgz";
      sha1 = "37930aa5d816b777c03445e1966cc6790a4c0b16";
    };
    deps = {
      "glob-5.0.15" = self.by-version."glob"."5.0.15";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."fined"."^1.0.1" =
    self.by-version."fined"."1.0.1";
  by-version."fined"."1.0.1" = self.buildNodePackage {
    name = "fined-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/fined/-/fined-1.0.1.tgz";
      name = "fined-1.0.1.tgz";
      sha1 = "c48af9ab5a8e0f400a0375e84154c37674dabfd4";
    };
    deps = {
      "expand-tilde-1.2.2" = self.by-version."expand-tilde"."1.2.2";
      "lodash.assignwith-4.2.0" = self.by-version."lodash.assignwith"."4.2.0";
      "lodash.isarray-4.0.0" = self.by-version."lodash.isarray"."4.0.0";
      "lodash.isempty-4.4.0" = self.by-version."lodash.isempty"."4.4.0";
      "lodash.isplainobject-4.0.6" = self.by-version."lodash.isplainobject"."4.0.6";
      "lodash.isstring-4.0.1" = self.by-version."lodash.isstring"."4.0.1";
      "lodash.pick-4.4.0" = self.by-version."lodash.pick"."4.4.0";
      "parse-filepath-1.0.1" = self.by-version."parse-filepath"."1.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."first-chunk-stream"."^1.0.0" =
    self.by-version."first-chunk-stream"."1.0.0";
  by-version."first-chunk-stream"."1.0.0" = self.buildNodePackage {
    name = "first-chunk-stream-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/first-chunk-stream/-/first-chunk-stream-1.0.0.tgz";
      name = "first-chunk-stream-1.0.0.tgz";
      sha1 = "59bfb50cd905f60d7c394cd3d9acaab4e6ad934e";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."flagged-respawn"."^0.3.2" =
    self.by-version."flagged-respawn"."0.3.2";
  by-version."flagged-respawn"."0.3.2" = self.buildNodePackage {
    name = "flagged-respawn-0.3.2";
    version = "0.3.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/flagged-respawn/-/flagged-respawn-0.3.2.tgz";
      name = "flagged-respawn-0.3.2.tgz";
      sha1 = "ff191eddcd7088a675b2610fffc976be9b8074b5";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."flatten"."0.0.1" =
    self.by-version."flatten"."0.0.1";
  by-version."flatten"."0.0.1" = self.buildNodePackage {
    name = "flatten-0.0.1";
    version = "0.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/flatten/-/flatten-0.0.1.tgz";
      name = "flatten-0.0.1.tgz";
      sha1 = "554440766da0a0d603999f433453f6c2fc6a75c1";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."follow-redirects"."0.0.3" =
    self.by-version."follow-redirects"."0.0.3";
  by-version."follow-redirects"."0.0.3" = self.buildNodePackage {
    name = "follow-redirects-0.0.3";
    version = "0.0.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/follow-redirects/-/follow-redirects-0.0.3.tgz";
      name = "follow-redirects-0.0.3.tgz";
      sha1 = "6ce67a24db1fe13f226c1171a72a7ef2b17b8f65";
    };
    deps = {
      "underscore-1.8.3" = self.by-version."underscore"."1.8.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."for-in"."^0.1.5" =
    self.by-version."for-in"."0.1.5";
  by-version."for-in"."0.1.5" = self.buildNodePackage {
    name = "for-in-0.1.5";
    version = "0.1.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/for-in/-/for-in-0.1.5.tgz";
      name = "for-in-0.1.5.tgz";
      sha1 = "007374e2b6d5c67420a1479bdb75a04872b738c4";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."for-own"."^0.1.3" =
    self.by-version."for-own"."0.1.4";
  by-version."for-own"."0.1.4" = self.buildNodePackage {
    name = "for-own-0.1.4";
    version = "0.1.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/for-own/-/for-own-0.1.4.tgz";
      name = "for-own-0.1.4.tgz";
      sha1 = "0149b41a39088c7515f51ebe1c1386d45f935072";
    };
    deps = {
      "for-in-0.1.5" = self.by-version."for-in"."0.1.5";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."forever-agent"."~0.5.0" =
    self.by-version."forever-agent"."0.5.2";
  by-version."forever-agent"."0.5.2" = self.buildNodePackage {
    name = "forever-agent-0.5.2";
    version = "0.5.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/forever-agent/-/forever-agent-0.5.2.tgz";
      name = "forever-agent-0.5.2.tgz";
      sha1 = "6d0e09c4921f94a27f63d3b49c5feff1ea4c5130";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."forever-agent"."~0.6.1" =
    self.by-version."forever-agent"."0.6.1";
  by-version."forever-agent"."0.6.1" = self.buildNodePackage {
    name = "forever-agent-0.6.1";
    version = "0.6.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/forever-agent/-/forever-agent-0.6.1.tgz";
      name = "forever-agent-0.6.1.tgz";
      sha1 = "fbc71f0c41adeb37f96c577ad1ed42d8fdacca91";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."fork-stream"."^0.0.4" =
    self.by-version."fork-stream"."0.0.4";
  by-version."fork-stream"."0.0.4" = self.buildNodePackage {
    name = "fork-stream-0.0.4";
    version = "0.0.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/fork-stream/-/fork-stream-0.0.4.tgz";
      name = "fork-stream-0.0.4.tgz";
      sha1 = "db849fce77f6708a5f8f386ae533a0907b54ae70";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."form-data"."1.0.0-rc3" =
    self.by-version."form-data"."1.0.0-rc3";
  by-version."form-data"."1.0.0-rc3" = self.buildNodePackage {
    name = "form-data-1.0.0-rc3";
    version = "1.0.0-rc3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/form-data/-/form-data-1.0.0-rc3.tgz";
      name = "form-data-1.0.0-rc3.tgz";
      sha1 = "d35bc62e7fbc2937ae78f948aaa0d38d90607577";
    };
    deps = {
      "async-1.5.2" = self.by-version."async"."1.5.2";
      "combined-stream-1.0.5" = self.by-version."combined-stream"."1.0.5";
      "mime-types-2.1.11" = self.by-version."mime-types"."2.1.11";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."form-data"."~0.1.0" =
    self.by-version."form-data"."0.1.4";
  by-version."form-data"."0.1.4" = self.buildNodePackage {
    name = "form-data-0.1.4";
    version = "0.1.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/form-data/-/form-data-0.1.4.tgz";
      name = "form-data-0.1.4.tgz";
      sha1 = "91abd788aba9702b1aabfa8bc01031a2ac9e3b12";
    };
    deps = {
      "combined-stream-0.0.7" = self.by-version."combined-stream"."0.0.7";
      "mime-1.2.11" = self.by-version."mime"."1.2.11";
      "async-0.9.2" = self.by-version."async"."0.9.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."form-data"."~1.0.0-rc3" =
    self.by-version."form-data"."1.0.0-rc4";
  by-version."form-data"."1.0.0-rc4" = self.buildNodePackage {
    name = "form-data-1.0.0-rc4";
    version = "1.0.0-rc4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/form-data/-/form-data-1.0.0-rc4.tgz";
      name = "form-data-1.0.0-rc4.tgz";
      sha1 = "05ac6bc22227b43e4461f488161554699d4f8b5e";
    };
    deps = {
      "async-1.5.2" = self.by-version."async"."1.5.2";
      "combined-stream-1.0.5" = self.by-version."combined-stream"."1.0.5";
      "mime-types-2.1.11" = self.by-version."mime-types"."2.1.11";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."form-data"."~1.0.0-rc4" =
    self.by-version."form-data"."1.0.0-rc4";
  by-spec."formidable"."1.0.x" =
    self.by-version."formidable"."1.0.17";
  by-version."formidable"."1.0.17" = self.buildNodePackage {
    name = "formidable-1.0.17";
    version = "1.0.17";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/formidable/-/formidable-1.0.17.tgz";
      name = "formidable-1.0.17.tgz";
      sha1 = "ef5491490f9433b705faa77249c99029ae348559";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."formidable"."~1.0.14" =
    self.by-version."formidable"."1.0.17";
  by-spec."foundation-emails"."^2.2.0" =
    self.by-version."foundation-emails"."2.2.1";
  by-version."foundation-emails"."2.2.1" = self.buildNodePackage {
    name = "foundation-emails-2.2.1";
    version = "2.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/foundation-emails/-/foundation-emails-2.2.1.tgz";
      name = "foundation-emails-2.2.1.tgz";
      sha1 = "1f3442762395d063d5a62d790740b2433aab1747";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."foundation-emails"."~2.2.1" =
    self.by-version."foundation-emails"."2.2.1";
  "foundation-emails" = self.by-version."foundation-emails"."2.2.1";
  by-spec."foxy"."^11.1.2" =
    self.by-version."foxy"."11.1.5";
  by-version."foxy"."11.1.5" = self.buildNodePackage {
    name = "foxy-11.1.5";
    version = "11.1.5";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/foxy/-/foxy-11.1.5.tgz";
      name = "foxy-11.1.5.tgz";
      sha1 = "b7e07d8cea9287f029298007083f4987c8aab458";
    };
    deps = {
      "connect-3.4.1" = self.by-version."connect"."3.4.1";
      "dev-ip-1.0.1" = self.by-version."dev-ip"."1.0.1";
      "eazy-logger-2.1.3" = self.by-version."eazy-logger"."2.1.3";
      "http-proxy-1.14.0" = self.by-version."http-proxy"."1.14.0";
      "lodash.merge-3.3.2" = self.by-version."lodash.merge"."3.3.2";
      "meow-3.7.0" = self.by-version."meow"."3.7.0";
      "resp-modifier-4.0.4" = self.by-version."resp-modifier"."4.0.4";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."fresh"."0.3.0" =
    self.by-version."fresh"."0.3.0";
  by-version."fresh"."0.3.0" = self.buildNodePackage {
    name = "fresh-0.3.0";
    version = "0.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/fresh/-/fresh-0.3.0.tgz";
      name = "fresh-0.3.0.tgz";
      sha1 = "651f838e22424e7566de161d8358caa199f83d4f";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."fresh"."^0.3.0" =
    self.by-version."fresh"."0.3.0";
  by-spec."from"."~0" =
    self.by-version."from"."0.1.3";
  by-version."from"."0.1.3" = self.buildNodePackage {
    name = "from-0.1.3";
    version = "0.1.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/from/-/from-0.1.3.tgz";
      name = "from-0.1.3.tgz";
      sha1 = "ef63ac2062ac32acf7862e0d40b44b896f22f3bc";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."front-matter"."^2.0.5" =
    self.by-version."front-matter"."2.1.0";
  by-version."front-matter"."2.1.0" = self.buildNodePackage {
    name = "front-matter-2.1.0";
    version = "2.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/front-matter/-/front-matter-2.1.0.tgz";
      name = "front-matter-2.1.0.tgz";
      sha1 = "0bdff42cbad2b35c07ac7085811789759f9858c0";
    };
    deps = {
      "js-yaml-3.6.1" = self.by-version."js-yaml"."3.6.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."fs-exists-sync"."^0.1.0" =
    self.by-version."fs-exists-sync"."0.1.0";
  by-version."fs-exists-sync"."0.1.0" = self.buildNodePackage {
    name = "fs-exists-sync-0.1.0";
    version = "0.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/fs-exists-sync/-/fs-exists-sync-0.1.0.tgz";
      name = "fs-exists-sync-0.1.0.tgz";
      sha1 = "982d6893af918e72d08dec9e8673ff2b5a8d6add";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."fs-extra"."^0.26.2" =
    self.by-version."fs-extra"."0.26.7";
  by-version."fs-extra"."0.26.7" = self.buildNodePackage {
    name = "fs-extra-0.26.7";
    version = "0.26.7";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/fs-extra/-/fs-extra-0.26.7.tgz";
      name = "fs-extra-0.26.7.tgz";
      sha1 = "9ae1fdd94897798edab76d0918cf42d0c3184fa9";
    };
    deps = {
      "graceful-fs-4.1.5" = self.by-version."graceful-fs"."4.1.5";
      "jsonfile-2.3.1" = self.by-version."jsonfile"."2.3.1";
      "klaw-1.3.0" = self.by-version."klaw"."1.3.0";
      "path-is-absolute-1.0.0" = self.by-version."path-is-absolute"."1.0.0";
      "rimraf-2.5.4" = self.by-version."rimraf"."2.5.4";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."fs.realpath"."^1.0.0" =
    self.by-version."fs.realpath"."1.0.0";
  by-version."fs.realpath"."1.0.0" = self.buildNodePackage {
    name = "fs.realpath-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/fs.realpath/-/fs.realpath-1.0.0.tgz";
      name = "fs.realpath-1.0.0.tgz";
      sha1 = "1504ad2523158caa40db4a2787cb01411994ea4f";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."fsevents"."^1.0.0" =
    self.by-version."fsevents"."1.0.14";
  by-version."fsevents"."1.0.14" = self.buildNodePackage {
    name = "fsevents-1.0.14";
    version = "1.0.14";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/fsevents/-/fsevents-1.0.14.tgz";
      name = "fsevents-1.0.14.tgz";
      sha1 = "558e8cc38643d8ef40fe45158486d0d25758eee4";
    };
    deps = {
      "nan-2.4.0" = self.by-version."nan"."2.4.0";
      "node-pre-gyp-0.6.29" = self.by-version."node-pre-gyp"."0.6.29";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ "darwin" ];
    cpu = [ ];
  };
  by-spec."fstream"."^1.0.0" =
    self.by-version."fstream"."1.0.10";
  by-version."fstream"."1.0.10" = self.buildNodePackage {
    name = "fstream-1.0.10";
    version = "1.0.10";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/fstream/-/fstream-1.0.10.tgz";
      name = "fstream-1.0.10.tgz";
      sha1 = "604e8a92fe26ffd9f6fae30399d4984e1ab22822";
    };
    deps = {
      "graceful-fs-4.1.5" = self.by-version."graceful-fs"."4.1.5";
      "inherits-2.0.1" = self.by-version."inherits"."2.0.1";
      "mkdirp-0.5.1" = self.by-version."mkdirp"."0.5.1";
      "rimraf-2.5.4" = self.by-version."rimraf"."2.5.4";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."fstream"."^1.0.2" =
    self.by-version."fstream"."1.0.10";
  by-spec."fstream"."~1.0.10" =
    self.by-version."fstream"."1.0.10";
  by-spec."fstream-ignore"."~1.0.5" =
    self.by-version."fstream-ignore"."1.0.5";
  by-version."fstream-ignore"."1.0.5" = self.buildNodePackage {
    name = "fstream-ignore-1.0.5";
    version = "1.0.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/fstream-ignore/-/fstream-ignore-1.0.5.tgz";
      name = "fstream-ignore-1.0.5.tgz";
      sha1 = "9c31dae34767018fe1d249b24dada67d092da105";
    };
    deps = {
      "fstream-1.0.10" = self.by-version."fstream"."1.0.10";
      "inherits-2.0.1" = self.by-version."inherits"."2.0.1";
      "minimatch-3.0.3" = self.by-version."minimatch"."3.0.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."gauge"."~2.6.0" =
    self.by-version."gauge"."2.6.0";
  by-version."gauge"."2.6.0" = self.buildNodePackage {
    name = "gauge-2.6.0";
    version = "2.6.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/gauge/-/gauge-2.6.0.tgz";
      name = "gauge-2.6.0.tgz";
      sha1 = "d35301ad18e96902b4751dcbbe40f4218b942a46";
    };
    deps = {
      "aproba-1.0.4" = self.by-version."aproba"."1.0.4";
      "console-control-strings-1.1.0" = self.by-version."console-control-strings"."1.1.0";
      "has-color-0.1.7" = self.by-version."has-color"."0.1.7";
      "has-unicode-2.0.1" = self.by-version."has-unicode"."2.0.1";
      "object-assign-4.1.0" = self.by-version."object-assign"."4.1.0";
      "signal-exit-3.0.0" = self.by-version."signal-exit"."3.0.0";
      "string-width-1.0.2" = self.by-version."string-width"."1.0.2";
      "strip-ansi-3.0.1" = self.by-version."strip-ansi"."3.0.1";
      "wide-align-1.1.0" = self.by-version."wide-align"."1.1.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."gaze"."^0.5.1" =
    self.by-version."gaze"."0.5.2";
  by-version."gaze"."0.5.2" = self.buildNodePackage {
    name = "gaze-0.5.2";
    version = "0.5.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/gaze/-/gaze-0.5.2.tgz";
      name = "gaze-0.5.2.tgz";
      sha1 = "40b709537d24d1d45767db5a908689dfe69ac44f";
    };
    deps = {
      "globule-0.1.0" = self.by-version."globule"."0.1.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."gaze"."^1.0.0" =
    self.by-version."gaze"."1.1.1";
  by-version."gaze"."1.1.1" = self.buildNodePackage {
    name = "gaze-1.1.1";
    version = "1.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/gaze/-/gaze-1.1.1.tgz";
      name = "gaze-1.1.1.tgz";
      sha1 = "ab81d557d1b515f5752bd5f1117d6fa3c4e9db41";
    };
    deps = {
      "globule-1.0.0" = self.by-version."globule"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."generate-function"."^2.0.0" =
    self.by-version."generate-function"."2.0.0";
  by-version."generate-function"."2.0.0" = self.buildNodePackage {
    name = "generate-function-2.0.0";
    version = "2.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/generate-function/-/generate-function-2.0.0.tgz";
      name = "generate-function-2.0.0.tgz";
      sha1 = "6858fe7c0969b7d4e9093337647ac79f60dfbe74";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."generate-object-property"."^1.1.0" =
    self.by-version."generate-object-property"."1.2.0";
  by-version."generate-object-property"."1.2.0" = self.buildNodePackage {
    name = "generate-object-property-1.2.0";
    version = "1.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/generate-object-property/-/generate-object-property-1.2.0.tgz";
      name = "generate-object-property-1.2.0.tgz";
      sha1 = "9c0e1c40308ce804f4783618b937fa88f99d50d0";
    };
    deps = {
      "is-property-1.0.2" = self.by-version."is-property"."1.0.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."get-caller-file"."^1.0.1" =
    self.by-version."get-caller-file"."1.0.2";
  by-version."get-caller-file"."1.0.2" = self.buildNodePackage {
    name = "get-caller-file-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/get-caller-file/-/get-caller-file-1.0.2.tgz";
      name = "get-caller-file-1.0.2.tgz";
      sha1 = "f702e63127e7e231c160a80c1554acb70d5047e5";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."get-proxy"."^1.0.1" =
    self.by-version."get-proxy"."1.1.0";
  by-version."get-proxy"."1.1.0" = self.buildNodePackage {
    name = "get-proxy-1.1.0";
    version = "1.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/get-proxy/-/get-proxy-1.1.0.tgz";
      name = "get-proxy-1.1.0.tgz";
      sha1 = "894854491bc591b0f147d7ae570f5c678b7256eb";
    };
    deps = {
      "rc-1.1.6" = self.by-version."rc"."1.1.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."get-stdin"."^4.0.1" =
    self.by-version."get-stdin"."4.0.1";
  by-version."get-stdin"."4.0.1" = self.buildNodePackage {
    name = "get-stdin-4.0.1";
    version = "4.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/get-stdin/-/get-stdin-4.0.1.tgz";
      name = "get-stdin-4.0.1.tgz";
      sha1 = "b968c6b0a04384324902e8bf1a5df32579a450fe";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."getpass"."^0.1.1" =
    self.by-version."getpass"."0.1.6";
  by-version."getpass"."0.1.6" = self.buildNodePackage {
    name = "getpass-0.1.6";
    version = "0.1.6";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/getpass/-/getpass-0.1.6.tgz";
      name = "getpass-0.1.6.tgz";
      sha1 = "283ffd9fc1256840875311c1b60e8c40187110e6";
    };
    deps = {
      "assert-plus-1.0.0" = self.by-version."assert-plus"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."gifsicle"."^3.0.0" =
    self.by-version."gifsicle"."3.0.3";
  by-version."gifsicle"."3.0.3" = self.buildNodePackage {
    name = "gifsicle-3.0.3";
    version = "3.0.3";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/gifsicle/-/gifsicle-3.0.3.tgz";
      name = "gifsicle-3.0.3.tgz";
      sha1 = "17230d311c68cbcf31bd54e22741e5c8a373117c";
    };
    deps = {
      "bin-build-2.2.0" = self.by-version."bin-build"."2.2.0";
      "bin-wrapper-3.0.2" = self.by-version."bin-wrapper"."3.0.2";
      "logalot-2.1.0" = self.by-version."logalot"."2.1.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."glob"."^4.3.1" =
    self.by-version."glob"."4.5.3";
  by-version."glob"."4.5.3" = self.buildNodePackage {
    name = "glob-4.5.3";
    version = "4.5.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/glob/-/glob-4.5.3.tgz";
      name = "glob-4.5.3.tgz";
      sha1 = "c6cb73d3226c1efef04de3c56d012f03377ee15f";
    };
    deps = {
      "inflight-1.0.5" = self.by-version."inflight"."1.0.5";
      "inherits-2.0.1" = self.by-version."inherits"."2.0.1";
      "minimatch-2.0.10" = self.by-version."minimatch"."2.0.10";
      "once-1.3.3" = self.by-version."once"."1.3.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."glob"."^4.4.2" =
    self.by-version."glob"."4.5.3";
  by-spec."glob"."^5.0.3" =
    self.by-version."glob"."5.0.15";
  by-version."glob"."5.0.15" = self.buildNodePackage {
    name = "glob-5.0.15";
    version = "5.0.15";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/glob/-/glob-5.0.15.tgz";
      name = "glob-5.0.15.tgz";
      sha1 = "1bc936b9e02f4a603fcc222ecf7633d30b8b93b1";
    };
    deps = {
      "inflight-1.0.5" = self.by-version."inflight"."1.0.5";
      "inherits-2.0.1" = self.by-version."inherits"."2.0.1";
      "minimatch-3.0.3" = self.by-version."minimatch"."3.0.3";
      "once-1.3.3" = self.by-version."once"."1.3.3";
      "path-is-absolute-1.0.0" = self.by-version."path-is-absolute"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."glob"."^7.0.0" =
    self.by-version."glob"."7.0.5";
  by-version."glob"."7.0.5" = self.buildNodePackage {
    name = "glob-7.0.5";
    version = "7.0.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/glob/-/glob-7.0.5.tgz";
      name = "glob-7.0.5.tgz";
      sha1 = "b4202a69099bbb4d292a7c1b95b6682b67ebdc95";
    };
    deps = {
      "fs.realpath-1.0.0" = self.by-version."fs.realpath"."1.0.0";
      "inflight-1.0.5" = self.by-version."inflight"."1.0.5";
      "inherits-2.0.1" = self.by-version."inherits"."2.0.1";
      "minimatch-3.0.3" = self.by-version."minimatch"."3.0.3";
      "once-1.3.3" = self.by-version."once"."1.3.3";
      "path-is-absolute-1.0.0" = self.by-version."path-is-absolute"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."glob"."^7.0.3" =
    self.by-version."glob"."7.0.5";
  by-spec."glob"."^7.0.5" =
    self.by-version."glob"."7.0.5";
  by-spec."glob"."~ 3.2.1" =
    self.by-version."glob"."3.2.11";
  by-version."glob"."3.2.11" = self.buildNodePackage {
    name = "glob-3.2.11";
    version = "3.2.11";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/glob/-/glob-3.2.11.tgz";
      name = "glob-3.2.11.tgz";
      sha1 = "4a973f635b9190f715d10987d5c00fd2815ebe3d";
    };
    deps = {
      "inherits-2.0.1" = self.by-version."inherits"."2.0.1";
      "minimatch-0.3.0" = self.by-version."minimatch"."0.3.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."glob"."~3.1.21" =
    self.by-version."glob"."3.1.21";
  by-version."glob"."3.1.21" = self.buildNodePackage {
    name = "glob-3.1.21";
    version = "3.1.21";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/glob/-/glob-3.1.21.tgz";
      name = "glob-3.1.21.tgz";
      sha1 = "d29e0a055dea5138f4d07ed40e8982e83c2066cd";
    };
    deps = {
      "minimatch-0.2.14" = self.by-version."minimatch"."0.2.14";
      "graceful-fs-1.2.3" = self.by-version."graceful-fs"."1.2.3";
      "inherits-1.0.2" = self.by-version."inherits"."1.0.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."glob"."~4.3.0" =
    self.by-version."glob"."4.3.5";
  by-version."glob"."4.3.5" = self.buildNodePackage {
    name = "glob-4.3.5";
    version = "4.3.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/glob/-/glob-4.3.5.tgz";
      name = "glob-4.3.5.tgz";
      sha1 = "80fbb08ca540f238acce5d11d1e9bc41e75173d3";
    };
    deps = {
      "inflight-1.0.5" = self.by-version."inflight"."1.0.5";
      "inherits-2.0.1" = self.by-version."inherits"."2.0.1";
      "minimatch-2.0.10" = self.by-version."minimatch"."2.0.10";
      "once-1.3.3" = self.by-version."once"."1.3.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."glob"."~5.0.0" =
    self.by-version."glob"."5.0.15";
  by-spec."glob"."~7.0.3" =
    self.by-version."glob"."7.0.5";
  by-spec."glob-base"."^0.3.0" =
    self.by-version."glob-base"."0.3.0";
  by-version."glob-base"."0.3.0" = self.buildNodePackage {
    name = "glob-base-0.3.0";
    version = "0.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/glob-base/-/glob-base-0.3.0.tgz";
      name = "glob-base-0.3.0.tgz";
      sha1 = "dbb164f6221b1c0b1ccf82aea328b497df0ea3c4";
    };
    deps = {
      "glob-parent-2.0.0" = self.by-version."glob-parent"."2.0.0";
      "is-glob-2.0.1" = self.by-version."is-glob"."2.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."glob-parent"."^2.0.0" =
    self.by-version."glob-parent"."2.0.0";
  by-version."glob-parent"."2.0.0" = self.buildNodePackage {
    name = "glob-parent-2.0.0";
    version = "2.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/glob-parent/-/glob-parent-2.0.0.tgz";
      name = "glob-parent-2.0.0.tgz";
      sha1 = "81383d72db054fcccf5336daa902f182f6edbb28";
    };
    deps = {
      "is-glob-2.0.1" = self.by-version."is-glob"."2.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."glob-stream"."^3.1.5" =
    self.by-version."glob-stream"."3.1.18";
  by-version."glob-stream"."3.1.18" = self.buildNodePackage {
    name = "glob-stream-3.1.18";
    version = "3.1.18";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/glob-stream/-/glob-stream-3.1.18.tgz";
      name = "glob-stream-3.1.18.tgz";
      sha1 = "9170a5f12b790306fdfe598f313f8f7954fd143b";
    };
    deps = {
      "glob-4.5.3" = self.by-version."glob"."4.5.3";
      "minimatch-2.0.10" = self.by-version."minimatch"."2.0.10";
      "ordered-read-streams-0.1.0" = self.by-version."ordered-read-streams"."0.1.0";
      "glob2base-0.0.12" = self.by-version."glob2base"."0.0.12";
      "unique-stream-1.0.0" = self.by-version."unique-stream"."1.0.0";
      "through2-0.6.5" = self.by-version."through2"."0.6.5";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."glob-stream"."^5.3.2" =
    self.by-version."glob-stream"."5.3.2";
  by-version."glob-stream"."5.3.2" = self.buildNodePackage {
    name = "glob-stream-5.3.2";
    version = "5.3.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/glob-stream/-/glob-stream-5.3.2.tgz";
      name = "glob-stream-5.3.2.tgz";
      sha1 = "cdfdaf7c3243cd53430a84dc934fa39d8c5da1a5";
    };
    deps = {
      "extend-3.0.0" = self.by-version."extend"."3.0.0";
      "glob-5.0.15" = self.by-version."glob"."5.0.15";
      "glob-parent-2.0.0" = self.by-version."glob-parent"."2.0.0";
      "micromatch-2.3.11" = self.by-version."micromatch"."2.3.11";
      "ordered-read-streams-0.3.0" = self.by-version."ordered-read-streams"."0.3.0";
      "through2-0.6.5" = self.by-version."through2"."0.6.5";
      "to-absolute-glob-0.1.1" = self.by-version."to-absolute-glob"."0.1.1";
      "unique-stream-2.2.1" = self.by-version."unique-stream"."2.2.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."glob-watcher"."^0.0.6" =
    self.by-version."glob-watcher"."0.0.6";
  by-version."glob-watcher"."0.0.6" = self.buildNodePackage {
    name = "glob-watcher-0.0.6";
    version = "0.0.6";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/glob-watcher/-/glob-watcher-0.0.6.tgz";
      name = "glob-watcher-0.0.6.tgz";
      sha1 = "b95b4a8df74b39c83298b0c05c978b4d9a3b710b";
    };
    deps = {
      "gaze-0.5.2" = self.by-version."gaze"."0.5.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."glob2base"."^0.0.12" =
    self.by-version."glob2base"."0.0.12";
  by-version."glob2base"."0.0.12" = self.buildNodePackage {
    name = "glob2base-0.0.12";
    version = "0.0.12";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/glob2base/-/glob2base-0.0.12.tgz";
      name = "glob2base-0.0.12.tgz";
      sha1 = "9d419b3e28f12e83a362164a277055922c9c0d56";
    };
    deps = {
      "find-index-0.1.1" = self.by-version."find-index"."0.1.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."global-modules"."^0.2.3" =
    self.by-version."global-modules"."0.2.3";
  by-version."global-modules"."0.2.3" = self.buildNodePackage {
    name = "global-modules-0.2.3";
    version = "0.2.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/global-modules/-/global-modules-0.2.3.tgz";
      name = "global-modules-0.2.3.tgz";
      sha1 = "ea5a3bed42c6d6ce995a4f8a1269b5dae223828d";
    };
    deps = {
      "global-prefix-0.1.4" = self.by-version."global-prefix"."0.1.4";
      "is-windows-0.2.0" = self.by-version."is-windows"."0.2.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."global-prefix"."^0.1.4" =
    self.by-version."global-prefix"."0.1.4";
  by-version."global-prefix"."0.1.4" = self.buildNodePackage {
    name = "global-prefix-0.1.4";
    version = "0.1.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/global-prefix/-/global-prefix-0.1.4.tgz";
      name = "global-prefix-0.1.4.tgz";
      sha1 = "05158db1cde2dd491b455e290eb3ab8bfc45c6e1";
    };
    deps = {
      "ini-1.3.4" = self.by-version."ini"."1.3.4";
      "is-windows-0.2.0" = self.by-version."is-windows"."0.2.0";
      "osenv-0.1.3" = self.by-version."osenv"."0.1.3";
      "which-1.2.10" = self.by-version."which"."1.2.10";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."globals"."^8.3.0" =
    self.by-version."globals"."8.18.0";
  by-version."globals"."8.18.0" = self.buildNodePackage {
    name = "globals-8.18.0";
    version = "8.18.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/globals/-/globals-8.18.0.tgz";
      name = "globals-8.18.0.tgz";
      sha1 = "93d4a62bdcac38cfafafc47d6b034768cb0ffcb4";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."globule"."^1.0.0" =
    self.by-version."globule"."1.0.0";
  by-version."globule"."1.0.0" = self.buildNodePackage {
    name = "globule-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/globule/-/globule-1.0.0.tgz";
      name = "globule-1.0.0.tgz";
      sha1 = "f22aebaacce02be492453e979c3ae9b6983f1c6c";
    };
    deps = {
      "glob-7.0.5" = self.by-version."glob"."7.0.5";
      "lodash-4.9.0" = self.by-version."lodash"."4.9.0";
      "minimatch-3.0.3" = self.by-version."minimatch"."3.0.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."globule"."~0.1.0" =
    self.by-version."globule"."0.1.0";
  by-version."globule"."0.1.0" = self.buildNodePackage {
    name = "globule-0.1.0";
    version = "0.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/globule/-/globule-0.1.0.tgz";
      name = "globule-0.1.0.tgz";
      sha1 = "d9c8edde1da79d125a151b79533b978676346ae5";
    };
    deps = {
      "lodash-1.0.2" = self.by-version."lodash"."1.0.2";
      "glob-3.1.21" = self.by-version."glob"."3.1.21";
      "minimatch-0.2.14" = self.by-version."minimatch"."0.2.14";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."glogg"."^1.0.0" =
    self.by-version."glogg"."1.0.0";
  by-version."glogg"."1.0.0" = self.buildNodePackage {
    name = "glogg-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/glogg/-/glogg-1.0.0.tgz";
      name = "glogg-1.0.0.tgz";
      sha1 = "7fe0f199f57ac906cf512feead8f90ee4a284fc5";
    };
    deps = {
      "sparkles-1.0.0" = self.by-version."sparkles"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."got"."^5.0.0" =
    self.by-version."got"."5.6.0";
  by-version."got"."5.6.0" = self.buildNodePackage {
    name = "got-5.6.0";
    version = "5.6.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/got/-/got-5.6.0.tgz";
      name = "got-5.6.0.tgz";
      sha1 = "bb1d7ee163b78082bbc8eb836f3f395004ea6fbf";
    };
    deps = {
      "create-error-class-3.0.2" = self.by-version."create-error-class"."3.0.2";
      "duplexer2-0.1.4" = self.by-version."duplexer2"."0.1.4";
      "is-plain-obj-1.1.0" = self.by-version."is-plain-obj"."1.1.0";
      "is-redirect-1.0.0" = self.by-version."is-redirect"."1.0.0";
      "is-retry-allowed-1.1.0" = self.by-version."is-retry-allowed"."1.1.0";
      "is-stream-1.1.0" = self.by-version."is-stream"."1.1.0";
      "lowercase-keys-1.0.0" = self.by-version."lowercase-keys"."1.0.0";
      "node-status-codes-1.0.0" = self.by-version."node-status-codes"."1.0.0";
      "object-assign-4.1.0" = self.by-version."object-assign"."4.1.0";
      "parse-json-2.2.0" = self.by-version."parse-json"."2.2.0";
      "pinkie-promise-2.0.1" = self.by-version."pinkie-promise"."2.0.1";
      "read-all-stream-3.1.0" = self.by-version."read-all-stream"."3.1.0";
      "readable-stream-2.1.5" = self.by-version."readable-stream"."2.1.5";
      "timed-out-2.0.0" = self.by-version."timed-out"."2.0.0";
      "unzip-response-1.0.0" = self.by-version."unzip-response"."1.0.0";
      "url-parse-lax-1.0.0" = self.by-version."url-parse-lax"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."graceful-fs"."^3.0.0" =
    self.by-version."graceful-fs"."3.0.9";
  by-version."graceful-fs"."3.0.9" = self.buildNodePackage {
    name = "graceful-fs-3.0.9";
    version = "3.0.9";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/graceful-fs/-/graceful-fs-3.0.9.tgz";
      name = "graceful-fs-3.0.9.tgz";
      sha1 = "44e10a870a068e892485bace909520905b08ba24";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."graceful-fs"."^4.0.0" =
    self.by-version."graceful-fs"."4.1.5";
  by-version."graceful-fs"."4.1.5" = self.buildNodePackage {
    name = "graceful-fs-4.1.5";
    version = "4.1.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/graceful-fs/-/graceful-fs-4.1.5.tgz";
      name = "graceful-fs-4.1.5.tgz";
      sha1 = "f4745e8caed5e0dd2ef21bb5e2d229a32e8093c0";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."graceful-fs"."^4.1.2" =
    self.by-version."graceful-fs"."4.1.5";
  by-spec."graceful-fs"."~1.2.0" =
    self.by-version."graceful-fs"."1.2.3";
  by-version."graceful-fs"."1.2.3" = self.buildNodePackage {
    name = "graceful-fs-1.2.3";
    version = "1.2.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/graceful-fs/-/graceful-fs-1.2.3.tgz";
      name = "graceful-fs-1.2.3.tgz";
      sha1 = "15a4806a57547cb2d2dbf27f42e89a8c3451b364";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."graceful-readlink".">= 1.0.0" =
    self.by-version."graceful-readlink"."1.0.1";
  by-version."graceful-readlink"."1.0.1" = self.buildNodePackage {
    name = "graceful-readlink-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/graceful-readlink/-/graceful-readlink-1.0.1.tgz";
      name = "graceful-readlink-1.0.1.tgz";
      sha1 = "4cafad76bc62f02fa039b2f94e9a3dd3a391a725";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."gulp"."~3.9.1" =
    self.by-version."gulp"."3.9.1";
  by-version."gulp"."3.9.1" = self.buildNodePackage {
    name = "gulp-3.9.1";
    version = "3.9.1";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/gulp/-/gulp-3.9.1.tgz";
      name = "gulp-3.9.1.tgz";
      sha1 = "571ce45928dd40af6514fc4011866016c13845b4";
    };
    deps = {
      "archy-1.0.0" = self.by-version."archy"."1.0.0";
      "chalk-1.1.3" = self.by-version."chalk"."1.1.3";
      "deprecated-0.0.1" = self.by-version."deprecated"."0.0.1";
      "gulp-util-3.0.7" = self.by-version."gulp-util"."3.0.7";
      "interpret-1.0.1" = self.by-version."interpret"."1.0.1";
      "liftoff-2.3.0" = self.by-version."liftoff"."2.3.0";
      "minimist-1.2.0" = self.by-version."minimist"."1.2.0";
      "orchestrator-0.3.7" = self.by-version."orchestrator"."0.3.7";
      "pretty-hrtime-1.0.2" = self.by-version."pretty-hrtime"."1.0.2";
      "semver-4.3.6" = self.by-version."semver"."4.3.6";
      "tildify-1.2.0" = self.by-version."tildify"."1.2.0";
      "v8flags-2.0.11" = self.by-version."v8flags"."2.0.11";
      "vinyl-fs-0.3.14" = self.by-version."vinyl-fs"."0.3.14";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "gulp" = self.by-version."gulp"."3.9.1";
  by-spec."gulp-awspublish"."~3.0.1" =
    self.by-version."gulp-awspublish"."3.0.2";
  by-version."gulp-awspublish"."3.0.2" = self.buildNodePackage {
    name = "gulp-awspublish-3.0.2";
    version = "3.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/gulp-awspublish/-/gulp-awspublish-3.0.2.tgz";
      name = "gulp-awspublish-3.0.2.tgz";
      sha1 = "0b68bdde19e9a6dd9d8ce38af48d352094cbde43";
    };
    deps = {
      "aws-sdk-2.5.1" = self.by-version."aws-sdk"."2.5.1";
      "clone-0.2.0" = self.by-version."clone"."0.2.0";
      "gulp-util-2.2.20" = self.by-version."gulp-util"."2.2.20";
      "mime-1.3.4" = self.by-version."mime"."1.3.4";
      "pad-component-0.0.1" = self.by-version."pad-component"."0.0.1";
      "pascal-case-1.1.2" = self.by-version."pascal-case"."1.1.2";
      "through2-0.6.5" = self.by-version."through2"."0.6.5";
      "vinyl-0.4.6" = self.by-version."vinyl"."0.4.6";
      "xml-json-2.0.2" = self.by-version."xml-json"."2.0.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "gulp-awspublish" = self.by-version."gulp-awspublish"."3.0.2";
  by-spec."gulp-cli"."~1.1.0" =
    self.by-version."gulp-cli"."1.1.1";
  by-version."gulp-cli"."1.1.1" = self.buildNodePackage {
    name = "gulp-cli-1.1.1";
    version = "1.1.1";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/gulp-cli/-/gulp-cli-1.1.1.tgz";
      name = "gulp-cli-1.1.1.tgz";
      sha1 = "1a4aae2981c9ada356483dae4c2dbcbc18bc3699";
    };
    deps = {
      "archy-1.0.0" = self.by-version."archy"."1.0.0";
      "chalk-1.1.3" = self.by-version."chalk"."1.1.3";
      "fancy-log-1.2.0" = self.by-version."fancy-log"."1.2.0";
      "gulplog-1.0.0" = self.by-version."gulplog"."1.0.0";
      "interpret-1.0.1" = self.by-version."interpret"."1.0.1";
      "liftoff-2.3.0" = self.by-version."liftoff"."2.3.0";
      "matchdep-1.0.1" = self.by-version."matchdep"."1.0.1";
      "mute-stdout-1.0.0" = self.by-version."mute-stdout"."1.0.0";
      "pretty-hrtime-1.0.2" = self.by-version."pretty-hrtime"."1.0.2";
      "semver-greatest-satisfied-range-1.0.0" = self.by-version."semver-greatest-satisfied-range"."1.0.0";
      "tildify-1.2.0" = self.by-version."tildify"."1.2.0";
      "v8flags-2.0.11" = self.by-version."v8flags"."2.0.11";
      "wreck-6.3.0" = self.by-version."wreck"."6.3.0";
      "yargs-3.32.0" = self.by-version."yargs"."3.32.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "gulp-cli" = self.by-version."gulp-cli"."1.1.1";
  by-spec."gulp-decompress"."^1.2.0" =
    self.by-version."gulp-decompress"."1.2.0";
  by-version."gulp-decompress"."1.2.0" = self.buildNodePackage {
    name = "gulp-decompress-1.2.0";
    version = "1.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/gulp-decompress/-/gulp-decompress-1.2.0.tgz";
      name = "gulp-decompress-1.2.0.tgz";
      sha1 = "8eeb65a5e015f8ed8532cafe28454960626f0dc7";
    };
    deps = {
      "archive-type-3.2.0" = self.by-version."archive-type"."3.2.0";
      "decompress-3.0.0" = self.by-version."decompress"."3.0.0";
      "gulp-util-3.0.7" = self.by-version."gulp-util"."3.0.7";
      "readable-stream-2.1.5" = self.by-version."readable-stream"."2.1.5";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."gulp-html-src"."~1.0.0" =
    self.by-version."gulp-html-src"."1.0.0";
  by-version."gulp-html-src"."1.0.0" = self.buildNodePackage {
    name = "gulp-html-src-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/gulp-html-src/-/gulp-html-src-1.0.0.tgz";
      name = "gulp-html-src-1.0.0.tgz";
      sha1 = "f0744d86727b64029df95c3af9888a3c10ff6d8b";
    };
    deps = {
      "vinyl-0.2.3" = self.by-version."vinyl"."0.2.3";
      "through2-0.4.2" = self.by-version."through2"."0.4.2";
      "cheerio-0.13.1" = self.by-version."cheerio"."0.13.1";
      "extend-1.2.1" = self.by-version."extend"."1.2.1";
      "q-1.0.1" = self.by-version."q"."1.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "gulp-html-src" = self.by-version."gulp-html-src"."1.0.0";
  by-spec."gulp-htmlmin"."~1.1.1" =
    self.by-version."gulp-htmlmin"."1.1.4";
  by-version."gulp-htmlmin"."1.1.4" = self.buildNodePackage {
    name = "gulp-htmlmin-1.1.4";
    version = "1.1.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/gulp-htmlmin/-/gulp-htmlmin-1.1.4.tgz";
      name = "gulp-htmlmin-1.1.4.tgz";
      sha1 = "9b85c0ad2b6ab614ad06804c86e7ea13ca85c553";
    };
    deps = {
      "bufferstreams-1.1.1" = self.by-version."bufferstreams"."1.1.1";
      "gulp-util-3.0.7" = self.by-version."gulp-util"."3.0.7";
      "html-minifier-0.7.2" = self.by-version."html-minifier"."0.7.2";
      "object-assign-4.1.0" = self.by-version."object-assign"."4.1.0";
      "readable-stream-2.1.5" = self.by-version."readable-stream"."2.1.5";
      "tryit-1.0.2" = self.by-version."tryit"."1.0.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "gulp-htmlmin" = self.by-version."gulp-htmlmin"."1.1.4";
  by-spec."gulp-if"."~2.0.0" =
    self.by-version."gulp-if"."2.0.1";
  by-version."gulp-if"."2.0.1" = self.buildNodePackage {
    name = "gulp-if-2.0.1";
    version = "2.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/gulp-if/-/gulp-if-2.0.1.tgz";
      name = "gulp-if-2.0.1.tgz";
      sha1 = "55f3edf82cecdfcf6035aa5d0686979aab0dac28";
    };
    deps = {
      "gulp-match-1.0.2" = self.by-version."gulp-match"."1.0.2";
      "ternary-stream-2.0.0" = self.by-version."ternary-stream"."2.0.0";
      "through2-2.0.1" = self.by-version."through2"."2.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "gulp-if" = self.by-version."gulp-if"."2.0.1";
  by-spec."gulp-imagemin"."~2.4.0" =
    self.by-version."gulp-imagemin"."2.4.0";
  by-version."gulp-imagemin"."2.4.0" = self.buildNodePackage {
    name = "gulp-imagemin-2.4.0";
    version = "2.4.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/gulp-imagemin/-/gulp-imagemin-2.4.0.tgz";
      name = "gulp-imagemin-2.4.0.tgz";
      sha1 = "f85948a77af532b4115dc9fbfac04af863a758ba";
    };
    deps = {
      "chalk-1.1.3" = self.by-version."chalk"."1.1.3";
      "gulp-util-3.0.7" = self.by-version."gulp-util"."3.0.7";
      "imagemin-4.0.0" = self.by-version."imagemin"."4.0.0";
      "object-assign-4.1.0" = self.by-version."object-assign"."4.1.0";
      "plur-2.1.2" = self.by-version."plur"."2.1.2";
      "pretty-bytes-2.0.1" = self.by-version."pretty-bytes"."2.0.1";
      "through2-concurrent-1.1.1" = self.by-version."through2-concurrent"."1.1.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "gulp-imagemin" = self.by-version."gulp-imagemin"."2.4.0";
  by-spec."gulp-inline-css"."~3.0.0" =
    self.by-version."gulp-inline-css"."3.0.1";
  by-version."gulp-inline-css"."3.0.1" = self.buildNodePackage {
    name = "gulp-inline-css-3.0.1";
    version = "3.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/gulp-inline-css/-/gulp-inline-css-3.0.1.tgz";
      name = "gulp-inline-css-3.0.1.tgz";
      sha1 = "91bd21d97f3a7cc466017df62f0652530bb53b55";
    };
    deps = {
      "gulp-util-3.0.7" = self.by-version."gulp-util"."3.0.7";
      "inline-css-2.2.2" = self.by-version."inline-css"."2.2.2";
      "through2-0.6.5" = self.by-version."through2"."0.6.5";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "gulp-inline-css" = self.by-version."gulp-inline-css"."3.0.1";
  by-spec."gulp-litmus"."0.0.7" =
    self.by-version."gulp-litmus"."0.0.7";
  by-version."gulp-litmus"."0.0.7" = self.buildNodePackage {
    name = "gulp-litmus-0.0.7";
    version = "0.0.7";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/gulp-litmus/-/gulp-litmus-0.0.7.tgz";
      name = "gulp-litmus-0.0.7.tgz";
      sha1 = "569fedd73a550efe79867563c805b283dd1652db";
    };
    deps = {
      "gulp-util-2.2.20" = self.by-version."gulp-util"."2.2.20";
      "event-stream-3.1.7" = self.by-version."event-stream"."3.1.7";
      "litmus-api-0.3.2" = self.by-version."litmus-api"."0.3.2";
      "dateformat-1.0.12" = self.by-version."dateformat"."1.0.12";
      "chalk-0.4.0" = self.by-version."chalk"."0.4.0";
      "cli-table-0.3.1" = self.by-version."cli-table"."0.3.1";
      "lodash-2.4.2" = self.by-version."lodash"."2.4.2";
      "request-2.36.0" = self.by-version."request"."2.36.0";
      "cheerio-0.17.0" = self.by-version."cheerio"."0.17.0";
      "xmlbuilder-2.2.1" = self.by-version."xmlbuilder"."2.2.1";
      "nodemailer-0.7.1" = self.by-version."nodemailer"."0.7.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "gulp-litmus" = self.by-version."gulp-litmus"."0.0.7";
  by-spec."gulp-load-plugins"."~1.1.0" =
    self.by-version."gulp-load-plugins"."1.1.0";
  by-version."gulp-load-plugins"."1.1.0" = self.buildNodePackage {
    name = "gulp-load-plugins-1.1.0";
    version = "1.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/gulp-load-plugins/-/gulp-load-plugins-1.1.0.tgz";
      name = "gulp-load-plugins-1.1.0.tgz";
      sha1 = "4519a3d44f8cb3ec68c70f3fb6652b47314788c4";
    };
    deps = {
      "findup-sync-0.2.1" = self.by-version."findup-sync"."0.2.1";
      "gulp-util-3.0.7" = self.by-version."gulp-util"."3.0.7";
      "multimatch-2.0.0" = self.by-version."multimatch"."2.0.0";
      "resolve-1.1.7" = self.by-version."resolve"."1.1.7";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "gulp-load-plugins" = self.by-version."gulp-load-plugins"."1.1.0";
  by-spec."gulp-match"."^1.0.2" =
    self.by-version."gulp-match"."1.0.2";
  by-version."gulp-match"."1.0.2" = self.buildNodePackage {
    name = "gulp-match-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/gulp-match/-/gulp-match-1.0.2.tgz";
      name = "gulp-match-1.0.2.tgz";
      sha1 = "f94c88e946f9faf8a50cb5a6e1b95cb5eeb33632";
    };
    deps = {
      "minimatch-3.0.3" = self.by-version."minimatch"."3.0.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."gulp-rename"."^1.2.0" =
    self.by-version."gulp-rename"."1.2.2";
  by-version."gulp-rename"."1.2.2" = self.buildNodePackage {
    name = "gulp-rename-1.2.2";
    version = "1.2.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/gulp-rename/-/gulp-rename-1.2.2.tgz";
      name = "gulp-rename-1.2.2.tgz";
      sha1 = "3ad4428763f05e2764dec1c67d868db275687817";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."gulp-rename"."~1.2.2" =
    self.by-version."gulp-rename"."1.2.2";
  "gulp-rename" = self.by-version."gulp-rename"."1.2.2";
  by-spec."gulp-replace"."~0.5.4" =
    self.by-version."gulp-replace"."0.5.4";
  by-version."gulp-replace"."0.5.4" = self.buildNodePackage {
    name = "gulp-replace-0.5.4";
    version = "0.5.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/gulp-replace/-/gulp-replace-0.5.4.tgz";
      name = "gulp-replace-0.5.4.tgz";
      sha1 = "69a67914bbd13c562bff14f504a403796aa0daa9";
    };
    deps = {
      "istextorbinary-1.0.2" = self.by-version."istextorbinary"."1.0.2";
      "readable-stream-2.1.5" = self.by-version."readable-stream"."2.1.5";
      "replacestream-4.0.0" = self.by-version."replacestream"."4.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "gulp-replace" = self.by-version."gulp-replace"."0.5.4";
  by-spec."gulp-sass"."~2.1.0" =
    self.by-version."gulp-sass"."2.1.1";
  by-version."gulp-sass"."2.1.1" = self.buildNodePackage {
    name = "gulp-sass-2.1.1";
    version = "2.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/gulp-sass/-/gulp-sass-2.1.1.tgz";
      name = "gulp-sass-2.1.1.tgz";
      sha1 = "6b266adbb36991992df8a3906a821392d0e2fe75";
    };
    deps = {
      "gulp-util-3.0.7" = self.by-version."gulp-util"."3.0.7";
      "node-sass-3.8.0" = self.by-version."node-sass"."3.8.0";
      "object-assign-4.1.0" = self.by-version."object-assign"."4.1.0";
      "through2-2.0.1" = self.by-version."through2"."2.0.1";
      "vinyl-sourcemaps-apply-0.2.1" = self.by-version."vinyl-sourcemaps-apply"."0.2.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "gulp-sass" = self.by-version."gulp-sass"."2.1.1";
  by-spec."gulp-sourcemaps"."^1.5.2" =
    self.by-version."gulp-sourcemaps"."1.6.0";
  by-version."gulp-sourcemaps"."1.6.0" = self.buildNodePackage {
    name = "gulp-sourcemaps-1.6.0";
    version = "1.6.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/gulp-sourcemaps/-/gulp-sourcemaps-1.6.0.tgz";
      name = "gulp-sourcemaps-1.6.0.tgz";
      sha1 = "b86ff349d801ceb56e1d9e7dc7bbcb4b7dee600c";
    };
    deps = {
      "convert-source-map-1.3.0" = self.by-version."convert-source-map"."1.3.0";
      "graceful-fs-4.1.5" = self.by-version."graceful-fs"."4.1.5";
      "strip-bom-2.0.0" = self.by-version."strip-bom"."2.0.0";
      "through2-2.0.1" = self.by-version."through2"."2.0.1";
      "vinyl-1.2.0" = self.by-version."vinyl"."1.2.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."gulp-sourcemaps"."~1.6.0" =
    self.by-version."gulp-sourcemaps"."1.6.0";
  "gulp-sourcemaps" = self.by-version."gulp-sourcemaps"."1.6.0";
  by-spec."gulp-util"."*" =
    self.by-version."gulp-util"."3.0.7";
  by-version."gulp-util"."3.0.7" = self.buildNodePackage {
    name = "gulp-util-3.0.7";
    version = "3.0.7";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/gulp-util/-/gulp-util-3.0.7.tgz";
      name = "gulp-util-3.0.7.tgz";
      sha1 = "78925c4b8f8b49005ac01a011c557e6218941cbb";
    };
    deps = {
      "array-differ-1.0.0" = self.by-version."array-differ"."1.0.0";
      "array-uniq-1.0.3" = self.by-version."array-uniq"."1.0.3";
      "beeper-1.1.0" = self.by-version."beeper"."1.1.0";
      "chalk-1.1.3" = self.by-version."chalk"."1.1.3";
      "dateformat-1.0.12" = self.by-version."dateformat"."1.0.12";
      "fancy-log-1.2.0" = self.by-version."fancy-log"."1.2.0";
      "gulplog-1.0.0" = self.by-version."gulplog"."1.0.0";
      "has-gulplog-0.1.0" = self.by-version."has-gulplog"."0.1.0";
      "lodash._reescape-3.0.0" = self.by-version."lodash._reescape"."3.0.0";
      "lodash._reevaluate-3.0.0" = self.by-version."lodash._reevaluate"."3.0.0";
      "lodash._reinterpolate-3.0.0" = self.by-version."lodash._reinterpolate"."3.0.0";
      "lodash.template-3.6.2" = self.by-version."lodash.template"."3.6.2";
      "minimist-1.2.0" = self.by-version."minimist"."1.2.0";
      "multipipe-0.1.2" = self.by-version."multipipe"."0.1.2";
      "object-assign-3.0.0" = self.by-version."object-assign"."3.0.0";
      "replace-ext-0.0.1" = self.by-version."replace-ext"."0.0.1";
      "through2-2.0.1" = self.by-version."through2"."2.0.1";
      "vinyl-0.5.3" = self.by-version."vinyl"."0.5.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."gulp-util"."2.x" =
    self.by-version."gulp-util"."2.2.20";
  by-version."gulp-util"."2.2.20" = self.buildNodePackage {
    name = "gulp-util-2.2.20";
    version = "2.2.20";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/gulp-util/-/gulp-util-2.2.20.tgz";
      name = "gulp-util-2.2.20.tgz";
      sha1 = "d7146e5728910bd8f047a6b0b1e549bc22dbd64c";
    };
    deps = {
      "chalk-0.5.1" = self.by-version."chalk"."0.5.1";
      "dateformat-1.0.12" = self.by-version."dateformat"."1.0.12";
      "lodash._reinterpolate-2.4.1" = self.by-version."lodash._reinterpolate"."2.4.1";
      "lodash.template-2.4.1" = self.by-version."lodash.template"."2.4.1";
      "minimist-0.2.0" = self.by-version."minimist"."0.2.0";
      "multipipe-0.1.2" = self.by-version."multipipe"."0.1.2";
      "through2-0.5.1" = self.by-version."through2"."0.5.1";
      "vinyl-0.2.3" = self.by-version."vinyl"."0.2.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."gulp-util"."^3.0" =
    self.by-version."gulp-util"."3.0.7";
  by-spec."gulp-util"."^3.0.0" =
    self.by-version."gulp-util"."3.0.7";
  by-spec."gulp-util"."^3.0.1" =
    self.by-version."gulp-util"."3.0.7";
  by-spec."gulp-util"."^3.0.6" =
    self.by-version."gulp-util"."3.0.7";
  by-spec."gulp-util"."^3.0.7" =
    self.by-version."gulp-util"."3.0.7";
  by-spec."gulp-util"."~2.2.14" =
    self.by-version."gulp-util"."2.2.20";
  by-spec."gulp-zip"."~3.2.0" =
    self.by-version."gulp-zip"."3.2.0";
  by-version."gulp-zip"."3.2.0" = self.buildNodePackage {
    name = "gulp-zip-3.2.0";
    version = "3.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/gulp-zip/-/gulp-zip-3.2.0.tgz";
      name = "gulp-zip-3.2.0.tgz";
      sha1 = "ebd198dae6dc2d5f44d814569c8ec42118a93ef9";
    };
    deps = {
      "chalk-1.1.3" = self.by-version."chalk"."1.1.3";
      "concat-stream-1.5.1" = self.by-version."concat-stream"."1.5.1";
      "gulp-util-3.0.7" = self.by-version."gulp-util"."3.0.7";
      "through2-2.0.1" = self.by-version."through2"."2.0.1";
      "yazl-2.4.1" = self.by-version."yazl"."2.4.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "gulp-zip" = self.by-version."gulp-zip"."3.2.0";
  by-spec."gulplog"."^1.0.0" =
    self.by-version."gulplog"."1.0.0";
  by-version."gulplog"."1.0.0" = self.buildNodePackage {
    name = "gulplog-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/gulplog/-/gulplog-1.0.0.tgz";
      name = "gulplog-1.0.0.tgz";
      sha1 = "e28c4d45d05ecbbed818363ce8f9c5926229ffe5";
    };
    deps = {
      "glogg-1.0.0" = self.by-version."glogg"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."handlebars"."^4.0.5" =
    self.by-version."handlebars"."4.0.5";
  by-version."handlebars"."4.0.5" = self.buildNodePackage {
    name = "handlebars-4.0.5";
    version = "4.0.5";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/handlebars/-/handlebars-4.0.5.tgz";
      name = "handlebars-4.0.5.tgz";
      sha1 = "92c6ed6bb164110c50d4d8d0fbddc70806c6f8e7";
    };
    deps = {
      "async-1.5.2" = self.by-version."async"."1.5.2";
      "optimist-0.6.1" = self.by-version."optimist"."0.6.1";
      "source-map-0.4.4" = self.by-version."source-map"."0.4.4";
    };
    optionalDependencies = {
      "uglify-js-2.7.3" = self.by-version."uglify-js"."2.7.3";
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."har-validator"."~2.0.2" =
    self.by-version."har-validator"."2.0.6";
  by-version."har-validator"."2.0.6" = self.buildNodePackage {
    name = "har-validator-2.0.6";
    version = "2.0.6";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/har-validator/-/har-validator-2.0.6.tgz";
      name = "har-validator-2.0.6.tgz";
      sha1 = "cdcbc08188265ad119b6a5a7c8ab70eecfb5d27d";
    };
    deps = {
      "chalk-1.1.3" = self.by-version."chalk"."1.1.3";
      "commander-2.9.0" = self.by-version."commander"."2.9.0";
      "is-my-json-valid-2.13.1" = self.by-version."is-my-json-valid"."2.13.1";
      "pinkie-promise-2.0.1" = self.by-version."pinkie-promise"."2.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."har-validator"."~2.0.6" =
    self.by-version."har-validator"."2.0.6";
  by-spec."has-ansi"."^0.1.0" =
    self.by-version."has-ansi"."0.1.0";
  by-version."has-ansi"."0.1.0" = self.buildNodePackage {
    name = "has-ansi-0.1.0";
    version = "0.1.0";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/has-ansi/-/has-ansi-0.1.0.tgz";
      name = "has-ansi-0.1.0.tgz";
      sha1 = "84f265aae8c0e6a88a12d7022894b7568894c62e";
    };
    deps = {
      "ansi-regex-0.2.1" = self.by-version."ansi-regex"."0.2.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."has-ansi"."^2.0.0" =
    self.by-version."has-ansi"."2.0.0";
  by-version."has-ansi"."2.0.0" = self.buildNodePackage {
    name = "has-ansi-2.0.0";
    version = "2.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/has-ansi/-/has-ansi-2.0.0.tgz";
      name = "has-ansi-2.0.0.tgz";
      sha1 = "34f5049ce1ecdf2b0649af3ef24e45ed35416d91";
    };
    deps = {
      "ansi-regex-2.0.0" = self.by-version."ansi-regex"."2.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."has-binary"."0.1.6" =
    self.by-version."has-binary"."0.1.6";
  by-version."has-binary"."0.1.6" = self.buildNodePackage {
    name = "has-binary-0.1.6";
    version = "0.1.6";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/has-binary/-/has-binary-0.1.6.tgz";
      name = "has-binary-0.1.6.tgz";
      sha1 = "25326f39cfa4f616ad8787894e3af2cfbc7b6e10";
    };
    deps = {
      "isarray-0.0.1" = self.by-version."isarray"."0.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."has-binary"."0.1.7" =
    self.by-version."has-binary"."0.1.7";
  by-version."has-binary"."0.1.7" = self.buildNodePackage {
    name = "has-binary-0.1.7";
    version = "0.1.7";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/has-binary/-/has-binary-0.1.7.tgz";
      name = "has-binary-0.1.7.tgz";
      sha1 = "68e61eb16210c9545a0a5cce06a873912fe1e68c";
    };
    deps = {
      "isarray-0.0.1" = self.by-version."isarray"."0.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."has-color"."^0.1.7" =
    self.by-version."has-color"."0.1.7";
  by-version."has-color"."0.1.7" = self.buildNodePackage {
    name = "has-color-0.1.7";
    version = "0.1.7";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/has-color/-/has-color-0.1.7.tgz";
      name = "has-color-0.1.7.tgz";
      sha1 = "67144a5260c34fc3cca677d041daf52fe7b78b2f";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."has-color"."~0.1.0" =
    self.by-version."has-color"."0.1.7";
  by-spec."has-cors"."1.1.0" =
    self.by-version."has-cors"."1.1.0";
  by-version."has-cors"."1.1.0" = self.buildNodePackage {
    name = "has-cors-1.1.0";
    version = "1.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/has-cors/-/has-cors-1.1.0.tgz";
      name = "has-cors-1.1.0.tgz";
      sha1 = "5e474793f7ea9843d1bb99c23eef49ff126fff39";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."has-gulplog"."^0.1.0" =
    self.by-version."has-gulplog"."0.1.0";
  by-version."has-gulplog"."0.1.0" = self.buildNodePackage {
    name = "has-gulplog-0.1.0";
    version = "0.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/has-gulplog/-/has-gulplog-0.1.0.tgz";
      name = "has-gulplog-0.1.0.tgz";
      sha1 = "6414c82913697da51590397dafb12f22967811ce";
    };
    deps = {
      "sparkles-1.0.0" = self.by-version."sparkles"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."has-unicode"."^2.0.0" =
    self.by-version."has-unicode"."2.0.1";
  by-version."has-unicode"."2.0.1" = self.buildNodePackage {
    name = "has-unicode-2.0.1";
    version = "2.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/has-unicode/-/has-unicode-2.0.1.tgz";
      name = "has-unicode-2.0.1.tgz";
      sha1 = "e0e6fe6a28cf51138855e086d1691e771de2a8b9";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."hawk"."~1.0.0" =
    self.by-version."hawk"."1.0.0";
  by-version."hawk"."1.0.0" = self.buildNodePackage {
    name = "hawk-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/hawk/-/hawk-1.0.0.tgz";
      name = "hawk-1.0.0.tgz";
      sha1 = "b90bb169807285411da7ffcb8dd2598502d3b52d";
    };
    deps = {
      "hoek-0.9.1" = self.by-version."hoek"."0.9.1";
      "boom-0.4.2" = self.by-version."boom"."0.4.2";
      "cryptiles-0.2.2" = self.by-version."cryptiles"."0.2.2";
      "sntp-0.2.4" = self.by-version."sntp"."0.2.4";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."hawk"."~3.1.0" =
    self.by-version."hawk"."3.1.3";
  by-version."hawk"."3.1.3" = self.buildNodePackage {
    name = "hawk-3.1.3";
    version = "3.1.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/hawk/-/hawk-3.1.3.tgz";
      name = "hawk-3.1.3.tgz";
      sha1 = "078444bd7c1640b0fe540d2c9b73d59678e8e1c4";
    };
    deps = {
      "hoek-2.16.3" = self.by-version."hoek"."2.16.3";
      "boom-2.10.1" = self.by-version."boom"."2.10.1";
      "cryptiles-2.0.5" = self.by-version."cryptiles"."2.0.5";
      "sntp-1.0.9" = self.by-version."sntp"."1.0.9";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."hawk"."~3.1.3" =
    self.by-version."hawk"."3.1.3";
  by-spec."he"."~0.3.6" =
    self.by-version."he"."0.3.6";
  by-version."he"."0.3.6" = self.buildNodePackage {
    name = "he-0.3.6";
    version = "0.3.6";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/he/-/he-0.3.6.tgz";
      name = "he-0.3.6.tgz";
      sha1 = "9d7bc446e77963933301dd602d5731cb861135e0";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."highlight.js"."^8.9.1" =
    self.by-version."highlight.js"."8.9.1";
  by-version."highlight.js"."8.9.1" = self.buildNodePackage {
    name = "highlight.js-8.9.1";
    version = "8.9.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/highlight.js/-/highlight.js-8.9.1.tgz";
      name = "highlight.js-8.9.1.tgz";
      sha1 = "b8a9c5493212a9392f0222b649c9611497ebfb88";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."hoek"."0.9.x" =
    self.by-version."hoek"."0.9.1";
  by-version."hoek"."0.9.1" = self.buildNodePackage {
    name = "hoek-0.9.1";
    version = "0.9.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/hoek/-/hoek-0.9.1.tgz";
      name = "hoek-0.9.1.tgz";
      sha1 = "3d322462badf07716ea7eb85baf88079cddce505";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."hoek"."2.x.x" =
    self.by-version."hoek"."2.16.3";
  by-version."hoek"."2.16.3" = self.buildNodePackage {
    name = "hoek-2.16.3";
    version = "2.16.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/hoek/-/hoek-2.16.3.tgz";
      name = "hoek-2.16.3.tgz";
      sha1 = "20bb7403d3cea398e91dc4710a8ff1b8274a25ed";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."home-or-tmp"."^1.0.0" =
    self.by-version."home-or-tmp"."1.0.0";
  by-version."home-or-tmp"."1.0.0" = self.buildNodePackage {
    name = "home-or-tmp-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/home-or-tmp/-/home-or-tmp-1.0.0.tgz";
      name = "home-or-tmp-1.0.0.tgz";
      sha1 = "4b9f1e40800c3e50c6c27f781676afcce71f3985";
    };
    deps = {
      "os-tmpdir-1.0.1" = self.by-version."os-tmpdir"."1.0.1";
      "user-home-1.1.1" = self.by-version."user-home"."1.1.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."hosted-git-info"."^2.1.4" =
    self.by-version."hosted-git-info"."2.1.5";
  by-version."hosted-git-info"."2.1.5" = self.buildNodePackage {
    name = "hosted-git-info-2.1.5";
    version = "2.1.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/hosted-git-info/-/hosted-git-info-2.1.5.tgz";
      name = "hosted-git-info-2.1.5.tgz";
      sha1 = "0ba81d90da2e25ab34a332e6ec77936e1598118b";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."href-content"."^1.0.0" =
    self.by-version."href-content"."1.0.2";
  by-version."href-content"."1.0.2" = self.buildNodePackage {
    name = "href-content-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/href-content/-/href-content-1.0.2.tgz";
      name = "href-content-1.0.2.tgz";
      sha1 = "8d46228a3d55ce24303ddfb93340d0efb7dddb58";
    };
    deps = {
      "remote-content-1.0.1" = self.by-version."remote-content"."1.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."html-minifier"."^0.7.2" =
    self.by-version."html-minifier"."0.7.2";
  by-version."html-minifier"."0.7.2" = self.buildNodePackage {
    name = "html-minifier-0.7.2";
    version = "0.7.2";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/html-minifier/-/html-minifier-0.7.2.tgz";
      name = "html-minifier-0.7.2.tgz";
      sha1 = "2b7959b1051a481e71cd7c6e59a64272af895cfd";
    };
    deps = {
      "change-case-2.3.1" = self.by-version."change-case"."2.3.1";
      "clean-css-3.1.9" = self.by-version."clean-css"."3.1.9";
      "cli-0.6.6" = self.by-version."cli"."0.6.6";
      "concat-stream-1.4.10" = self.by-version."concat-stream"."1.4.10";
      "uglify-js-2.4.24" = self.by-version."uglify-js"."2.4.24";
      "relateurl-0.2.7" = self.by-version."relateurl"."0.2.7";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."htmlparser2"."~3.4.0" =
    self.by-version."htmlparser2"."3.4.0";
  by-version."htmlparser2"."3.4.0" = self.buildNodePackage {
    name = "htmlparser2-3.4.0";
    version = "3.4.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/htmlparser2/-/htmlparser2-3.4.0.tgz";
      name = "htmlparser2-3.4.0.tgz";
      sha1 = "a1cd65f5823ad285e19d63b085ad722d0a51eae7";
    };
    deps = {
      "domhandler-2.2.1" = self.by-version."domhandler"."2.2.1";
      "domutils-1.3.0" = self.by-version."domutils"."1.3.0";
      "domelementtype-1.3.0" = self.by-version."domelementtype"."1.3.0";
      "readable-stream-1.1.14" = self.by-version."readable-stream"."1.1.14";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."htmlparser2"."~3.7.2" =
    self.by-version."htmlparser2"."3.7.3";
  by-version."htmlparser2"."3.7.3" = self.buildNodePackage {
    name = "htmlparser2-3.7.3";
    version = "3.7.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/htmlparser2/-/htmlparser2-3.7.3.tgz";
      name = "htmlparser2-3.7.3.tgz";
      sha1 = "6a64c77637c08c6f30ec2a8157a53333be7cb05e";
    };
    deps = {
      "domhandler-2.2.1" = self.by-version."domhandler"."2.2.1";
      "domutils-1.5.1" = self.by-version."domutils"."1.5.1";
      "domelementtype-1.3.0" = self.by-version."domelementtype"."1.3.0";
      "readable-stream-1.1.14" = self.by-version."readable-stream"."1.1.14";
      "entities-1.0.0" = self.by-version."entities"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."htmlparser2"."~3.8.1" =
    self.by-version."htmlparser2"."3.8.3";
  by-version."htmlparser2"."3.8.3" = self.buildNodePackage {
    name = "htmlparser2-3.8.3";
    version = "3.8.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/htmlparser2/-/htmlparser2-3.8.3.tgz";
      name = "htmlparser2-3.8.3.tgz";
      sha1 = "996c28b191516a8be86501a7d79757e5c70c1068";
    };
    deps = {
      "domhandler-2.3.0" = self.by-version."domhandler"."2.3.0";
      "domutils-1.5.1" = self.by-version."domutils"."1.5.1";
      "domelementtype-1.3.0" = self.by-version."domelementtype"."1.3.0";
      "readable-stream-1.1.14" = self.by-version."readable-stream"."1.1.14";
      "entities-1.0.0" = self.by-version."entities"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."http-errors"."~1.5.0" =
    self.by-version."http-errors"."1.5.0";
  by-version."http-errors"."1.5.0" = self.buildNodePackage {
    name = "http-errors-1.5.0";
    version = "1.5.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/http-errors/-/http-errors-1.5.0.tgz";
      name = "http-errors-1.5.0.tgz";
      sha1 = "b1cb3d8260fd8e2386cad3189045943372d48211";
    };
    deps = {
      "inherits-2.0.1" = self.by-version."inherits"."2.0.1";
      "setprototypeof-1.0.1" = self.by-version."setprototypeof"."1.0.1";
      "statuses-1.3.0" = self.by-version."statuses"."1.3.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."http-proxy"."^1.9.0" =
    self.by-version."http-proxy"."1.14.0";
  by-version."http-proxy"."1.14.0" = self.buildNodePackage {
    name = "http-proxy-1.14.0";
    version = "1.14.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/http-proxy/-/http-proxy-1.14.0.tgz";
      name = "http-proxy-1.14.0.tgz";
      sha1 = "be32ab34dd5229e87840f4c27cb335ee195b2a83";
    };
    deps = {
      "eventemitter3-1.2.0" = self.by-version."eventemitter3"."1.2.0";
      "requires-port-1.0.0" = self.by-version."requires-port"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."http-signature"."~0.10.0" =
    self.by-version."http-signature"."0.10.1";
  by-version."http-signature"."0.10.1" = self.buildNodePackage {
    name = "http-signature-0.10.1";
    version = "0.10.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/http-signature/-/http-signature-0.10.1.tgz";
      name = "http-signature-0.10.1.tgz";
      sha1 = "4fbdac132559aa8323121e540779c0a012b27e66";
    };
    deps = {
      "assert-plus-0.1.5" = self.by-version."assert-plus"."0.1.5";
      "asn1-0.1.11" = self.by-version."asn1"."0.1.11";
      "ctype-0.5.3" = self.by-version."ctype"."0.5.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."http-signature"."~0.11.0" =
    self.by-version."http-signature"."0.11.0";
  by-version."http-signature"."0.11.0" = self.buildNodePackage {
    name = "http-signature-0.11.0";
    version = "0.11.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/http-signature/-/http-signature-0.11.0.tgz";
      name = "http-signature-0.11.0.tgz";
      sha1 = "1796cf67a001ad5cd6849dca0991485f09089fe6";
    };
    deps = {
      "assert-plus-0.1.5" = self.by-version."assert-plus"."0.1.5";
      "asn1-0.1.11" = self.by-version."asn1"."0.1.11";
      "ctype-0.5.3" = self.by-version."ctype"."0.5.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."http-signature"."~1.1.0" =
    self.by-version."http-signature"."1.1.1";
  by-version."http-signature"."1.1.1" = self.buildNodePackage {
    name = "http-signature-1.1.1";
    version = "1.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/http-signature/-/http-signature-1.1.1.tgz";
      name = "http-signature-1.1.1.tgz";
      sha1 = "df72e267066cd0ac67fb76adf8e134a8fbcf91bf";
    };
    deps = {
      "assert-plus-0.2.0" = self.by-version."assert-plus"."0.2.0";
      "jsprim-1.3.0" = self.by-version."jsprim"."1.3.0";
      "sshpk-1.9.2" = self.by-version."sshpk"."1.9.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."iconv-lite"."~0.4.13" =
    self.by-version."iconv-lite"."0.4.13";
  by-version."iconv-lite"."0.4.13" = self.buildNodePackage {
    name = "iconv-lite-0.4.13";
    version = "0.4.13";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/iconv-lite/-/iconv-lite-0.4.13.tgz";
      name = "iconv-lite-0.4.13.tgz";
      sha1 = "1f88aba4ab0b1508e8312acc39345f36e992e2f2";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."imagemin"."^4.0.0" =
    self.by-version."imagemin"."4.0.0";
  by-version."imagemin"."4.0.0" = self.buildNodePackage {
    name = "imagemin-4.0.0";
    version = "4.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/imagemin/-/imagemin-4.0.0.tgz";
      name = "imagemin-4.0.0.tgz";
      sha1 = "e90e7f0936836595f18fa15fe906f4fa259ea847";
    };
    deps = {
      "buffer-to-vinyl-1.1.0" = self.by-version."buffer-to-vinyl"."1.1.0";
      "concat-stream-1.5.1" = self.by-version."concat-stream"."1.5.1";
      "optional-0.1.3" = self.by-version."optional"."0.1.3";
      "readable-stream-2.1.5" = self.by-version."readable-stream"."2.1.5";
      "stream-combiner2-1.1.1" = self.by-version."stream-combiner2"."1.1.1";
      "vinyl-fs-2.4.3" = self.by-version."vinyl-fs"."2.4.3";
    };
    optionalDependencies = {
      "imagemin-gifsicle-4.2.0" = self.by-version."imagemin-gifsicle"."4.2.0";
      "imagemin-jpegtran-4.3.2" = self.by-version."imagemin-jpegtran"."4.3.2";
      "imagemin-optipng-4.3.0" = self.by-version."imagemin-optipng"."4.3.0";
      "imagemin-svgo-4.2.1" = self.by-version."imagemin-svgo"."4.2.1";
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."imagemin-gifsicle"."^4.0.0" =
    self.by-version."imagemin-gifsicle"."4.2.0";
  by-version."imagemin-gifsicle"."4.2.0" = self.buildNodePackage {
    name = "imagemin-gifsicle-4.2.0";
    version = "4.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/imagemin-gifsicle/-/imagemin-gifsicle-4.2.0.tgz";
      name = "imagemin-gifsicle-4.2.0.tgz";
      sha1 = "0fef9bbad3476e6b76885736cc5b0b87a08757ca";
    };
    deps = {
      "gifsicle-3.0.3" = self.by-version."gifsicle"."3.0.3";
      "is-gif-1.0.0" = self.by-version."is-gif"."1.0.0";
      "through2-0.6.5" = self.by-version."through2"."0.6.5";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."imagemin-jpegtran"."^4.0.0" =
    self.by-version."imagemin-jpegtran"."4.3.2";
  by-version."imagemin-jpegtran"."4.3.2" = self.buildNodePackage {
    name = "imagemin-jpegtran-4.3.2";
    version = "4.3.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/imagemin-jpegtran/-/imagemin-jpegtran-4.3.2.tgz";
      name = "imagemin-jpegtran-4.3.2.tgz";
      sha1 = "1bc6d1e2bd13fdb64d245526d635a7e5dfeb12fc";
    };
    deps = {
      "is-jpg-1.0.0" = self.by-version."is-jpg"."1.0.0";
      "jpegtran-bin-3.1.0" = self.by-version."jpegtran-bin"."3.1.0";
      "through2-2.0.1" = self.by-version."through2"."2.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."imagemin-optipng"."^4.0.0" =
    self.by-version."imagemin-optipng"."4.3.0";
  by-version."imagemin-optipng"."4.3.0" = self.buildNodePackage {
    name = "imagemin-optipng-4.3.0";
    version = "4.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/imagemin-optipng/-/imagemin-optipng-4.3.0.tgz";
      name = "imagemin-optipng-4.3.0.tgz";
      sha1 = "7604663ab2ee315733274726fd1c374d2b44adb6";
    };
    deps = {
      "exec-buffer-2.0.1" = self.by-version."exec-buffer"."2.0.1";
      "is-png-1.0.0" = self.by-version."is-png"."1.0.0";
      "optipng-bin-3.1.2" = self.by-version."optipng-bin"."3.1.2";
      "through2-0.6.5" = self.by-version."through2"."0.6.5";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."imagemin-svgo"."^4.0.0" =
    self.by-version."imagemin-svgo"."4.2.1";
  by-version."imagemin-svgo"."4.2.1" = self.buildNodePackage {
    name = "imagemin-svgo-4.2.1";
    version = "4.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/imagemin-svgo/-/imagemin-svgo-4.2.1.tgz";
      name = "imagemin-svgo-4.2.1.tgz";
      sha1 = "54f07dc56f47260462df6a61c54befb44b57be55";
    };
    deps = {
      "is-svg-1.1.1" = self.by-version."is-svg"."1.1.1";
      "svgo-0.6.6" = self.by-version."svgo"."0.6.6";
      "through2-2.0.1" = self.by-version."through2"."2.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."immutable"."^3.7.4" =
    self.by-version."immutable"."3.8.1";
  by-version."immutable"."3.8.1" = self.buildNodePackage {
    name = "immutable-3.8.1";
    version = "3.8.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/immutable/-/immutable-3.8.1.tgz";
      name = "immutable-3.8.1.tgz";
      sha1 = "200807f11ab0f72710ea485542de088075f68cd2";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."immutable"."^3.7.6" =
    self.by-version."immutable"."3.8.1";
  by-spec."in-publish"."^2.0.0" =
    self.by-version."in-publish"."2.0.0";
  by-version."in-publish"."2.0.0" = self.buildNodePackage {
    name = "in-publish-2.0.0";
    version = "2.0.0";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/in-publish/-/in-publish-2.0.0.tgz";
      name = "in-publish-2.0.0.tgz";
      sha1 = "e20ff5e3a2afc2690320b6dc552682a9c7fadf51";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."indent-string"."^1.1.0" =
    self.by-version."indent-string"."1.2.2";
  by-version."indent-string"."1.2.2" = self.buildNodePackage {
    name = "indent-string-1.2.2";
    version = "1.2.2";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/indent-string/-/indent-string-1.2.2.tgz";
      name = "indent-string-1.2.2.tgz";
      sha1 = "db99bcc583eb6abbb1e48dcbb1999a986041cb6b";
    };
    deps = {
      "get-stdin-4.0.1" = self.by-version."get-stdin"."4.0.1";
      "minimist-1.2.0" = self.by-version."minimist"."1.2.0";
      "repeating-1.1.3" = self.by-version."repeating"."1.1.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."indent-string"."^2.1.0" =
    self.by-version."indent-string"."2.1.0";
  by-version."indent-string"."2.1.0" = self.buildNodePackage {
    name = "indent-string-2.1.0";
    version = "2.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/indent-string/-/indent-string-2.1.0.tgz";
      name = "indent-string-2.1.0.tgz";
      sha1 = "8e2d48348742121b4a8218b7a137e9a52049dc80";
    };
    deps = {
      "repeating-2.0.1" = self.by-version."repeating"."2.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."indexof"."0.0.1" =
    self.by-version."indexof"."0.0.1";
  by-version."indexof"."0.0.1" = self.buildNodePackage {
    name = "indexof-0.0.1";
    version = "0.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/indexof/-/indexof-0.0.1.tgz";
      name = "indexof-0.0.1.tgz";
      sha1 = "82dc336d232b9062179d05ab3293a66059fd435d";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."inflight"."^1.0.4" =
    self.by-version."inflight"."1.0.5";
  by-version."inflight"."1.0.5" = self.buildNodePackage {
    name = "inflight-1.0.5";
    version = "1.0.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/inflight/-/inflight-1.0.5.tgz";
      name = "inflight-1.0.5.tgz";
      sha1 = "db3204cd5a9de2e6cd890b85c6e2f66bcf4f620a";
    };
    deps = {
      "once-1.3.3" = self.by-version."once"."1.3.3";
      "wrappy-1.0.2" = self.by-version."wrappy"."1.0.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."inherits"."1" =
    self.by-version."inherits"."1.0.2";
  by-version."inherits"."1.0.2" = self.buildNodePackage {
    name = "inherits-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/inherits/-/inherits-1.0.2.tgz";
      name = "inherits-1.0.2.tgz";
      sha1 = "ca4309dadee6b54cc0b8d247e8d7c7a0975bdc9b";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."inherits"."2" =
    self.by-version."inherits"."2.0.1";
  by-version."inherits"."2.0.1" = self.buildNodePackage {
    name = "inherits-2.0.1";
    version = "2.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/inherits/-/inherits-2.0.1.tgz";
      name = "inherits-2.0.1.tgz";
      sha1 = "b17d08d326b4423e568eff719f91b0b1cbdf69f1";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."inherits"."2.0.1" =
    self.by-version."inherits"."2.0.1";
  by-spec."inherits"."^2.0.1" =
    self.by-version."inherits"."2.0.1";
  by-spec."inherits"."~2.0.0" =
    self.by-version."inherits"."2.0.1";
  by-spec."inherits"."~2.0.1" =
    self.by-version."inherits"."2.0.1";
  by-spec."ini"."^1.3.4" =
    self.by-version."ini"."1.3.4";
  by-version."ini"."1.3.4" = self.buildNodePackage {
    name = "ini-1.3.4";
    version = "1.3.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/ini/-/ini-1.3.4.tgz";
      name = "ini-1.3.4.tgz";
      sha1 = "0537cb79daf59b59a1a517dff706c86ec039162e";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."ini"."~1.3.0" =
    self.by-version."ini"."1.3.4";
  by-spec."inky"."~1.3.6" =
    self.by-version."inky"."1.3.6";
  by-version."inky"."1.3.6" = self.buildNodePackage {
    name = "inky-1.3.6";
    version = "1.3.6";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/inky/-/inky-1.3.6.tgz";
      name = "inky-1.3.6.tgz";
      sha1 = "32c23d7eb7097d9dad07e21677a391e708b0b288";
    };
    deps = {
      "cheerio-0.20.0" = self.by-version."cheerio"."0.20.0";
      "foundation-emails-2.2.1" = self.by-version."foundation-emails"."2.2.1";
      "mkdirp-0.5.1" = self.by-version."mkdirp"."0.5.1";
      "multiline-1.0.2" = self.by-version."multiline"."1.0.2";
      "object-values-1.0.0" = self.by-version."object-values"."1.0.0";
      "through2-2.0.1" = self.by-version."through2"."2.0.1";
      "vinyl-fs-2.4.3" = self.by-version."vinyl-fs"."2.4.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "inky" = self.by-version."inky"."1.3.6";
  by-spec."inline-css"."^2.1.1" =
    self.by-version."inline-css"."2.2.2";
  by-version."inline-css"."2.2.2" = self.buildNodePackage {
    name = "inline-css-2.2.2";
    version = "2.2.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/inline-css/-/inline-css-2.2.2.tgz";
      name = "inline-css-2.2.2.tgz";
      sha1 = "da1e4b14e42a14ef55f9332b5ad1b2df657149a6";
    };
    deps = {
      "bluebird-2.10.2" = self.by-version."bluebird"."2.10.2";
      "cheerio-0.19.0" = self.by-version."cheerio"."0.19.0";
      "css-property-2.0.0" = self.by-version."css-property"."2.0.0";
      "css-rules-1.0.2" = self.by-version."css-rules"."1.0.2";
      "extend-3.0.0" = self.by-version."extend"."3.0.0";
      "extract-css-1.0.3" = self.by-version."extract-css"."1.0.3";
      "flatten-0.0.1" = self.by-version."flatten"."0.0.1";
      "object.pick-1.1.2" = self.by-version."object.pick"."1.1.2";
      "style-selector-2.0.1" = self.by-version."style-selector"."2.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."interpret"."^1.0.0" =
    self.by-version."interpret"."1.0.1";
  by-version."interpret"."1.0.1" = self.buildNodePackage {
    name = "interpret-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/interpret/-/interpret-1.0.1.tgz";
      name = "interpret-1.0.1.tgz";
      sha1 = "d579fb7f693b858004947af39fa0db49f795602c";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."invariant"."^2.2.0" =
    self.by-version."invariant"."2.2.1";
  by-version."invariant"."2.2.1" = self.buildNodePackage {
    name = "invariant-2.2.1";
    version = "2.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/invariant/-/invariant-2.2.1.tgz";
      name = "invariant-2.2.1.tgz";
      sha1 = "b097010547668c7e337028ebe816ebe36c8a8d54";
    };
    deps = {
      "loose-envify-1.2.0" = self.by-version."loose-envify"."1.2.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."invert-kv"."^1.0.0" =
    self.by-version."invert-kv"."1.0.0";
  by-version."invert-kv"."1.0.0" = self.buildNodePackage {
    name = "invert-kv-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/invert-kv/-/invert-kv-1.0.0.tgz";
      name = "invert-kv-1.0.0.tgz";
      sha1 = "104a8e4aaca6d3d8cd157a8ef8bfab2d7a3ffdb6";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."ip-regex"."^1.0.1" =
    self.by-version."ip-regex"."1.0.3";
  by-version."ip-regex"."1.0.3" = self.buildNodePackage {
    name = "ip-regex-1.0.3";
    version = "1.0.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/ip-regex/-/ip-regex-1.0.3.tgz";
      name = "ip-regex-1.0.3.tgz";
      sha1 = "dc589076f659f419c222039a33316f1c7387effd";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."irregular-plurals"."^1.0.0" =
    self.by-version."irregular-plurals"."1.2.0";
  by-version."irregular-plurals"."1.2.0" = self.buildNodePackage {
    name = "irregular-plurals-1.2.0";
    version = "1.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/irregular-plurals/-/irregular-plurals-1.2.0.tgz";
      name = "irregular-plurals-1.2.0.tgz";
      sha1 = "38f299834ba8c00c30be9c554e137269752ff3ac";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-absolute"."^0.1.5" =
    self.by-version."is-absolute"."0.1.7";
  by-version."is-absolute"."0.1.7" = self.buildNodePackage {
    name = "is-absolute-0.1.7";
    version = "0.1.7";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-absolute/-/is-absolute-0.1.7.tgz";
      name = "is-absolute-0.1.7.tgz";
      sha1 = "847491119fccb5fb436217cc737f7faad50f603f";
    };
    deps = {
      "is-relative-0.1.3" = self.by-version."is-relative"."0.1.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-absolute"."^0.2.3" =
    self.by-version."is-absolute"."0.2.5";
  by-version."is-absolute"."0.2.5" = self.buildNodePackage {
    name = "is-absolute-0.2.5";
    version = "0.2.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-absolute/-/is-absolute-0.2.5.tgz";
      name = "is-absolute-0.2.5.tgz";
      sha1 = "994142b9f468d27c14fbf0cd30fe77db934ca76d";
    };
    deps = {
      "is-relative-0.2.1" = self.by-version."is-relative"."0.2.1";
      "is-windows-0.1.1" = self.by-version."is-windows"."0.1.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-arrayish"."^0.2.1" =
    self.by-version."is-arrayish"."0.2.1";
  by-version."is-arrayish"."0.2.1" = self.buildNodePackage {
    name = "is-arrayish-0.2.1";
    version = "0.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-arrayish/-/is-arrayish-0.2.1.tgz";
      name = "is-arrayish-0.2.1.tgz";
      sha1 = "77c99840527aa8ecb1a8ba697b80645a7a926a9d";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-binary-path"."^1.0.0" =
    self.by-version."is-binary-path"."1.0.1";
  by-version."is-binary-path"."1.0.1" = self.buildNodePackage {
    name = "is-binary-path-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-binary-path/-/is-binary-path-1.0.1.tgz";
      name = "is-binary-path-1.0.1.tgz";
      sha1 = "75f16642b480f187a711c814161fd3a4a7655898";
    };
    deps = {
      "binary-extensions-1.5.0" = self.by-version."binary-extensions"."1.5.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-buffer"."^1.0.2" =
    self.by-version."is-buffer"."1.1.4";
  by-version."is-buffer"."1.1.4" = self.buildNodePackage {
    name = "is-buffer-1.1.4";
    version = "1.1.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-buffer/-/is-buffer-1.1.4.tgz";
      name = "is-buffer-1.1.4.tgz";
      sha1 = "cfc86ccd5dc5a52fa80489111c6920c457e2d98b";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-builtin-module"."^1.0.0" =
    self.by-version."is-builtin-module"."1.0.0";
  by-version."is-builtin-module"."1.0.0" = self.buildNodePackage {
    name = "is-builtin-module-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-builtin-module/-/is-builtin-module-1.0.0.tgz";
      name = "is-builtin-module-1.0.0.tgz";
      sha1 = "540572d34f7ac3119f8f76c30cbc1b1e037affbe";
    };
    deps = {
      "builtin-modules-1.1.1" = self.by-version."builtin-modules"."1.1.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-bzip2"."^1.0.0" =
    self.by-version."is-bzip2"."1.0.0";
  by-version."is-bzip2"."1.0.0" = self.buildNodePackage {
    name = "is-bzip2-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-bzip2/-/is-bzip2-1.0.0.tgz";
      name = "is-bzip2-1.0.0.tgz";
      sha1 = "5ee58eaa5a2e9c80e21407bedf23ae5ac091b3fc";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-dotfile"."^1.0.0" =
    self.by-version."is-dotfile"."1.0.2";
  by-version."is-dotfile"."1.0.2" = self.buildNodePackage {
    name = "is-dotfile-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-dotfile/-/is-dotfile-1.0.2.tgz";
      name = "is-dotfile-1.0.2.tgz";
      sha1 = "2c132383f39199f8edc268ca01b9b007d205cc4d";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-equal-shallow"."^0.1.3" =
    self.by-version."is-equal-shallow"."0.1.3";
  by-version."is-equal-shallow"."0.1.3" = self.buildNodePackage {
    name = "is-equal-shallow-0.1.3";
    version = "0.1.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-equal-shallow/-/is-equal-shallow-0.1.3.tgz";
      name = "is-equal-shallow-0.1.3.tgz";
      sha1 = "2238098fc221de0bcfa5d9eac4c45d638aa1c534";
    };
    deps = {
      "is-primitive-2.0.0" = self.by-version."is-primitive"."2.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-extendable"."^0.1.0" =
    self.by-version."is-extendable"."0.1.1";
  by-version."is-extendable"."0.1.1" = self.buildNodePackage {
    name = "is-extendable-0.1.1";
    version = "0.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-extendable/-/is-extendable-0.1.1.tgz";
      name = "is-extendable-0.1.1.tgz";
      sha1 = "62b110e289a471418e3ec36a617d472e301dfc89";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-extendable"."^0.1.1" =
    self.by-version."is-extendable"."0.1.1";
  by-spec."is-extglob"."^1.0.0" =
    self.by-version."is-extglob"."1.0.0";
  by-version."is-extglob"."1.0.0" = self.buildNodePackage {
    name = "is-extglob-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-extglob/-/is-extglob-1.0.0.tgz";
      name = "is-extglob-1.0.0.tgz";
      sha1 = "ac468177c4943405a092fc8f29760c6ffc6206c0";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-finite"."^1.0.0" =
    self.by-version."is-finite"."1.0.1";
  by-version."is-finite"."1.0.1" = self.buildNodePackage {
    name = "is-finite-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-finite/-/is-finite-1.0.1.tgz";
      name = "is-finite-1.0.1.tgz";
      sha1 = "6438603eaebe2793948ff4a4262ec8db3d62597b";
    };
    deps = {
      "number-is-nan-1.0.0" = self.by-version."number-is-nan"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-fullwidth-code-point"."^1.0.0" =
    self.by-version."is-fullwidth-code-point"."1.0.0";
  by-version."is-fullwidth-code-point"."1.0.0" = self.buildNodePackage {
    name = "is-fullwidth-code-point-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-fullwidth-code-point/-/is-fullwidth-code-point-1.0.0.tgz";
      name = "is-fullwidth-code-point-1.0.0.tgz";
      sha1 = "ef9e31386f031a7f0d643af82fde50c457ef00cb";
    };
    deps = {
      "number-is-nan-1.0.0" = self.by-version."number-is-nan"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-gif"."^1.0.0" =
    self.by-version."is-gif"."1.0.0";
  by-version."is-gif"."1.0.0" = self.buildNodePackage {
    name = "is-gif-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-gif/-/is-gif-1.0.0.tgz";
      name = "is-gif-1.0.0.tgz";
      sha1 = "a6d2ae98893007bffa97a1d8c01d63205832097e";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-glob"."^2.0.0" =
    self.by-version."is-glob"."2.0.1";
  by-version."is-glob"."2.0.1" = self.buildNodePackage {
    name = "is-glob-2.0.1";
    version = "2.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-glob/-/is-glob-2.0.1.tgz";
      name = "is-glob-2.0.1.tgz";
      sha1 = "d096f926a3ded5600f3fdfd91198cb0888c2d863";
    };
    deps = {
      "is-extglob-1.0.0" = self.by-version."is-extglob"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-glob"."^2.0.1" =
    self.by-version."is-glob"."2.0.1";
  by-spec."is-gzip"."^1.0.0" =
    self.by-version."is-gzip"."1.0.0";
  by-version."is-gzip"."1.0.0" = self.buildNodePackage {
    name = "is-gzip-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-gzip/-/is-gzip-1.0.0.tgz";
      name = "is-gzip-1.0.0.tgz";
      sha1 = "6ca8b07b99c77998025900e555ced8ed80879a83";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-jpg"."^1.0.0" =
    self.by-version."is-jpg"."1.0.0";
  by-version."is-jpg"."1.0.0" = self.buildNodePackage {
    name = "is-jpg-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-jpg/-/is-jpg-1.0.0.tgz";
      name = "is-jpg-1.0.0.tgz";
      sha1 = "2959c17e73430db38264da75b90dd54f2d86da1c";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-lower-case"."^1.1.0" =
    self.by-version."is-lower-case"."1.1.3";
  by-version."is-lower-case"."1.1.3" = self.buildNodePackage {
    name = "is-lower-case-1.1.3";
    version = "1.1.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-lower-case/-/is-lower-case-1.1.3.tgz";
      name = "is-lower-case-1.1.3.tgz";
      sha1 = "7e147be4768dc466db3bfb21cc60b31e6ad69393";
    };
    deps = {
      "lower-case-1.1.3" = self.by-version."lower-case"."1.1.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-my-json-valid"."^2.12.4" =
    self.by-version."is-my-json-valid"."2.13.1";
  by-version."is-my-json-valid"."2.13.1" = self.buildNodePackage {
    name = "is-my-json-valid-2.13.1";
    version = "2.13.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-my-json-valid/-/is-my-json-valid-2.13.1.tgz";
      name = "is-my-json-valid-2.13.1.tgz";
      sha1 = "d55778a82feb6b0963ff4be111d5d1684e890707";
    };
    deps = {
      "generate-function-2.0.0" = self.by-version."generate-function"."2.0.0";
      "generate-object-property-1.2.0" = self.by-version."generate-object-property"."1.2.0";
      "jsonpointer-2.0.0" = self.by-version."jsonpointer"."2.0.0";
      "xtend-4.0.1" = self.by-version."xtend"."4.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-natural-number"."^2.0.0" =
    self.by-version."is-natural-number"."2.1.1";
  by-version."is-natural-number"."2.1.1" = self.buildNodePackage {
    name = "is-natural-number-2.1.1";
    version = "2.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-natural-number/-/is-natural-number-2.1.1.tgz";
      name = "is-natural-number-2.1.1.tgz";
      sha1 = "7d4c5728377ef386c3e194a9911bf57c6dc335e7";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-number"."^2.0.2" =
    self.by-version."is-number"."2.1.0";
  by-version."is-number"."2.1.0" = self.buildNodePackage {
    name = "is-number-2.1.0";
    version = "2.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-number/-/is-number-2.1.0.tgz";
      name = "is-number-2.1.0.tgz";
      sha1 = "01fcbbb393463a548f2f466cce16dece49db908f";
    };
    deps = {
      "kind-of-3.0.4" = self.by-version."kind-of"."3.0.4";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-number"."^2.1.0" =
    self.by-version."is-number"."2.1.0";
  by-spec."is-obj"."^1.0.0" =
    self.by-version."is-obj"."1.0.1";
  by-version."is-obj"."1.0.1" = self.buildNodePackage {
    name = "is-obj-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-obj/-/is-obj-1.0.1.tgz";
      name = "is-obj-1.0.1.tgz";
      sha1 = "3e4729ac1f5fde025cd7d83a896dab9f4f67db0f";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-plain-obj"."^1.0.0" =
    self.by-version."is-plain-obj"."1.1.0";
  by-version."is-plain-obj"."1.1.0" = self.buildNodePackage {
    name = "is-plain-obj-1.1.0";
    version = "1.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-plain-obj/-/is-plain-obj-1.1.0.tgz";
      name = "is-plain-obj-1.1.0.tgz";
      sha1 = "71a50c8429dfca773c92a390a4a03b39fcd51d3e";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-png"."^1.0.0" =
    self.by-version."is-png"."1.0.0";
  by-version."is-png"."1.0.0" = self.buildNodePackage {
    name = "is-png-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-png/-/is-png-1.0.0.tgz";
      name = "is-png-1.0.0.tgz";
      sha1 = "3d80373fe9b89d65fd341f659d3fc0a1135e718a";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-posix-bracket"."^0.1.0" =
    self.by-version."is-posix-bracket"."0.1.1";
  by-version."is-posix-bracket"."0.1.1" = self.buildNodePackage {
    name = "is-posix-bracket-0.1.1";
    version = "0.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-posix-bracket/-/is-posix-bracket-0.1.1.tgz";
      name = "is-posix-bracket-0.1.1.tgz";
      sha1 = "3334dc79774368e92f016e6fbc0a88f5cd6e6bc4";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-primitive"."^2.0.0" =
    self.by-version."is-primitive"."2.0.0";
  by-version."is-primitive"."2.0.0" = self.buildNodePackage {
    name = "is-primitive-2.0.0";
    version = "2.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-primitive/-/is-primitive-2.0.0.tgz";
      name = "is-primitive-2.0.0.tgz";
      sha1 = "207bab91638499c07b2adf240a41a87210034575";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-property"."^1.0.0" =
    self.by-version."is-property"."1.0.2";
  by-version."is-property"."1.0.2" = self.buildNodePackage {
    name = "is-property-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-property/-/is-property-1.0.2.tgz";
      name = "is-property-1.0.2.tgz";
      sha1 = "57fe1c4e48474edd65b09911f26b1cd4095dda84";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-redirect"."^1.0.0" =
    self.by-version."is-redirect"."1.0.0";
  by-version."is-redirect"."1.0.0" = self.buildNodePackage {
    name = "is-redirect-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-redirect/-/is-redirect-1.0.0.tgz";
      name = "is-redirect-1.0.0.tgz";
      sha1 = "1d03dded53bd8db0f30c26e4f95d36fc7c87dc24";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-relative"."^0.1.0" =
    self.by-version."is-relative"."0.1.3";
  by-version."is-relative"."0.1.3" = self.buildNodePackage {
    name = "is-relative-0.1.3";
    version = "0.1.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-relative/-/is-relative-0.1.3.tgz";
      name = "is-relative-0.1.3.tgz";
      sha1 = "905fee8ae86f45b3ec614bc3c15c869df0876e82";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-relative"."^0.2.1" =
    self.by-version."is-relative"."0.2.1";
  by-version."is-relative"."0.2.1" = self.buildNodePackage {
    name = "is-relative-0.2.1";
    version = "0.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-relative/-/is-relative-0.2.1.tgz";
      name = "is-relative-0.2.1.tgz";
      sha1 = "d27f4c7d516d175fb610db84bbeef23c3bc97aa5";
    };
    deps = {
      "is-unc-path-0.1.1" = self.by-version."is-unc-path"."0.1.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-retry-allowed"."^1.0.0" =
    self.by-version."is-retry-allowed"."1.1.0";
  by-version."is-retry-allowed"."1.1.0" = self.buildNodePackage {
    name = "is-retry-allowed-1.1.0";
    version = "1.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-retry-allowed/-/is-retry-allowed-1.1.0.tgz";
      name = "is-retry-allowed-1.1.0.tgz";
      sha1 = "11a060568b67339444033d0125a61a20d564fb34";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-stream"."^1.0.0" =
    self.by-version."is-stream"."1.1.0";
  by-version."is-stream"."1.1.0" = self.buildNodePackage {
    name = "is-stream-1.1.0";
    version = "1.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-stream/-/is-stream-1.1.0.tgz";
      name = "is-stream-1.1.0.tgz";
      sha1 = "12d4a3dd4e68e0b79ceb8dbc84173ae80d91ca44";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-stream"."^1.0.1" =
    self.by-version."is-stream"."1.1.0";
  by-spec."is-svg"."^1.0.0" =
    self.by-version."is-svg"."1.1.1";
  by-version."is-svg"."1.1.1" = self.buildNodePackage {
    name = "is-svg-1.1.1";
    version = "1.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-svg/-/is-svg-1.1.1.tgz";
      name = "is-svg-1.1.1.tgz";
      sha1 = "ac0efaafb653ac58473708b1f873636ca110e31b";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-tar"."^1.0.0" =
    self.by-version."is-tar"."1.0.0";
  by-version."is-tar"."1.0.0" = self.buildNodePackage {
    name = "is-tar-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-tar/-/is-tar-1.0.0.tgz";
      name = "is-tar-1.0.0.tgz";
      sha1 = "2f6b2e1792c1f5bb36519acaa9d65c0d26fe853d";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-typedarray"."~1.0.0" =
    self.by-version."is-typedarray"."1.0.0";
  by-version."is-typedarray"."1.0.0" = self.buildNodePackage {
    name = "is-typedarray-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-typedarray/-/is-typedarray-1.0.0.tgz";
      name = "is-typedarray-1.0.0.tgz";
      sha1 = "e479c80858df0c1b11ddda6940f96011fcda4a9a";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-unc-path"."^0.1.1" =
    self.by-version."is-unc-path"."0.1.1";
  by-version."is-unc-path"."0.1.1" = self.buildNodePackage {
    name = "is-unc-path-0.1.1";
    version = "0.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-unc-path/-/is-unc-path-0.1.1.tgz";
      name = "is-unc-path-0.1.1.tgz";
      sha1 = "ab2533d77ad733561124c3dc0f5cd8b90054c86b";
    };
    deps = {
      "unc-path-regex-0.1.2" = self.by-version."unc-path-regex"."0.1.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-upper-case"."^1.1.0" =
    self.by-version."is-upper-case"."1.1.2";
  by-version."is-upper-case"."1.1.2" = self.buildNodePackage {
    name = "is-upper-case-1.1.2";
    version = "1.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-upper-case/-/is-upper-case-1.1.2.tgz";
      name = "is-upper-case-1.1.2.tgz";
      sha1 = "8d0b1fa7e7933a1e58483600ec7d9661cbaf756f";
    };
    deps = {
      "upper-case-1.1.3" = self.by-version."upper-case"."1.1.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-url"."^1.2.0" =
    self.by-version."is-url"."1.2.2";
  by-version."is-url"."1.2.2" = self.buildNodePackage {
    name = "is-url-1.2.2";
    version = "1.2.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-url/-/is-url-1.2.2.tgz";
      name = "is-url-1.2.2.tgz";
      sha1 = "498905a593bf47cc2d9e7f738372bbf7696c7f26";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-utf8"."^0.2.0" =
    self.by-version."is-utf8"."0.2.1";
  by-version."is-utf8"."0.2.1" = self.buildNodePackage {
    name = "is-utf8-0.2.1";
    version = "0.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-utf8/-/is-utf8-0.2.1.tgz";
      name = "is-utf8-0.2.1.tgz";
      sha1 = "4b0da1442104d1b336340e80797e865cf39f7d72";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-valid-glob"."^0.3.0" =
    self.by-version."is-valid-glob"."0.3.0";
  by-version."is-valid-glob"."0.3.0" = self.buildNodePackage {
    name = "is-valid-glob-0.3.0";
    version = "0.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-valid-glob/-/is-valid-glob-0.3.0.tgz";
      name = "is-valid-glob-0.3.0.tgz";
      sha1 = "d4b55c69f51886f9b65c70d6c2622d37e29f48fe";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-windows"."^0.1.1" =
    self.by-version."is-windows"."0.1.1";
  by-version."is-windows"."0.1.1" = self.buildNodePackage {
    name = "is-windows-0.1.1";
    version = "0.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-windows/-/is-windows-0.1.1.tgz";
      name = "is-windows-0.1.1.tgz";
      sha1 = "be310715431cfabccc54ab3951210fa0b6d01abe";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-windows"."^0.2.0" =
    self.by-version."is-windows"."0.2.0";
  by-version."is-windows"."0.2.0" = self.buildNodePackage {
    name = "is-windows-0.2.0";
    version = "0.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-windows/-/is-windows-0.2.0.tgz";
      name = "is-windows-0.2.0.tgz";
      sha1 = "de1aa6d63ea29dd248737b69f1ff8b8002d2108c";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-zip"."^1.0.0" =
    self.by-version."is-zip"."1.0.0";
  by-version."is-zip"."1.0.0" = self.buildNodePackage {
    name = "is-zip-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-zip/-/is-zip-1.0.0.tgz";
      name = "is-zip-1.0.0.tgz";
      sha1 = "47b0a8ff4d38a76431ccfd99a8e15a4c86ba2325";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."isarray"."0.0.1" =
    self.by-version."isarray"."0.0.1";
  by-version."isarray"."0.0.1" = self.buildNodePackage {
    name = "isarray-0.0.1";
    version = "0.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/isarray/-/isarray-0.0.1.tgz";
      name = "isarray-0.0.1.tgz";
      sha1 = "8a18acfca9a8f4177e09abfc6038939b05d1eedf";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."isarray"."1.0.0" =
    self.by-version."isarray"."1.0.0";
  by-version."isarray"."1.0.0" = self.buildNodePackage {
    name = "isarray-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/isarray/-/isarray-1.0.0.tgz";
      name = "isarray-1.0.0.tgz";
      sha1 = "bb935d48582cba168c06834957a54a3e07124f11";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."isarray"."~1.0.0" =
    self.by-version."isarray"."1.0.0";
  by-spec."isexe"."^1.1.1" =
    self.by-version."isexe"."1.1.2";
  by-version."isexe"."1.1.2" = self.buildNodePackage {
    name = "isexe-1.1.2";
    version = "1.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/isexe/-/isexe-1.1.2.tgz";
      name = "isexe-1.1.2.tgz";
      sha1 = "36f3e22e60750920f5e7241a476a8c6a42275ad0";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."isobject"."^2.0.0" =
    self.by-version."isobject"."2.1.0";
  by-version."isobject"."2.1.0" = self.buildNodePackage {
    name = "isobject-2.1.0";
    version = "2.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/isobject/-/isobject-2.1.0.tgz";
      name = "isobject-2.1.0.tgz";
      sha1 = "f065561096a3f1da2ef46272f815c840d87e0c89";
    };
    deps = {
      "isarray-1.0.0" = self.by-version."isarray"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."isstream"."~0.1.2" =
    self.by-version."isstream"."0.1.2";
  by-version."isstream"."0.1.2" = self.buildNodePackage {
    name = "isstream-0.1.2";
    version = "0.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/isstream/-/isstream-0.1.2.tgz";
      name = "isstream-0.1.2.tgz";
      sha1 = "47e63f7af55afa6f92e1500e690eb8b8529c099a";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."istextorbinary"."1.0.2" =
    self.by-version."istextorbinary"."1.0.2";
  by-version."istextorbinary"."1.0.2" = self.buildNodePackage {
    name = "istextorbinary-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/istextorbinary/-/istextorbinary-1.0.2.tgz";
      name = "istextorbinary-1.0.2.tgz";
      sha1 = "ace19354d1a9a0173efeb1084ce0f87b0ad7decf";
    };
    deps = {
      "textextensions-1.0.2" = self.by-version."textextensions"."1.0.2";
      "binaryextensions-1.0.1" = self.by-version."binaryextensions"."1.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."jmespath"."0.15.0" =
    self.by-version."jmespath"."0.15.0";
  by-version."jmespath"."0.15.0" = self.buildNodePackage {
    name = "jmespath-0.15.0";
    version = "0.15.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/jmespath/-/jmespath-0.15.0.tgz";
      name = "jmespath-0.15.0.tgz";
      sha1 = "a3f222a9aae9f966f5d27c796510e28091764217";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."jodid25519"."^1.0.0" =
    self.by-version."jodid25519"."1.0.2";
  by-version."jodid25519"."1.0.2" = self.buildNodePackage {
    name = "jodid25519-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/jodid25519/-/jodid25519-1.0.2.tgz";
      name = "jodid25519-1.0.2.tgz";
      sha1 = "06d4912255093419477d425633606e0e90782967";
    };
    deps = {
      "jsbn-0.1.0" = self.by-version."jsbn"."0.1.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."jpegtran-bin"."^3.0.0" =
    self.by-version."jpegtran-bin"."3.1.0";
  by-version."jpegtran-bin"."3.1.0" = self.buildNodePackage {
    name = "jpegtran-bin-3.1.0";
    version = "3.1.0";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/jpegtran-bin/-/jpegtran-bin-3.1.0.tgz";
      name = "jpegtran-bin-3.1.0.tgz";
      sha1 = "e6541eba8d97448282a1f8f80a1b6cc9feea55d0";
    };
    deps = {
      "bin-build-2.2.0" = self.by-version."bin-build"."2.2.0";
      "bin-wrapper-3.0.2" = self.by-version."bin-wrapper"."3.0.2";
      "logalot-2.1.0" = self.by-version."logalot"."2.1.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."js-tokens"."^1.0.1" =
    self.by-version."js-tokens"."1.0.3";
  by-version."js-tokens"."1.0.3" = self.buildNodePackage {
    name = "js-tokens-1.0.3";
    version = "1.0.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/js-tokens/-/js-tokens-1.0.3.tgz";
      name = "js-tokens-1.0.3.tgz";
      sha1 = "14e56eb68c8f1a92c43d59f5014ec29dc20f2ae1";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."js-tokens"."^2.0.0" =
    self.by-version."js-tokens"."2.0.0";
  by-version."js-tokens"."2.0.0" = self.buildNodePackage {
    name = "js-tokens-2.0.0";
    version = "2.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/js-tokens/-/js-tokens-2.0.0.tgz";
      name = "js-tokens-2.0.0.tgz";
      sha1 = "79903f5563ee778cc1162e6dcf1a0027c97f9cb5";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."js-yaml"."^3.4.6" =
    self.by-version."js-yaml"."3.6.1";
  by-version."js-yaml"."3.6.1" = self.buildNodePackage {
    name = "js-yaml-3.6.1";
    version = "3.6.1";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/js-yaml/-/js-yaml-3.6.1.tgz";
      name = "js-yaml-3.6.1.tgz";
      sha1 = "6e5fe67d8b205ce4d22fad05b7781e8dadcc4b30";
    };
    deps = {
      "argparse-1.0.7" = self.by-version."argparse"."1.0.7";
      "esprima-2.7.2" = self.by-version."esprima"."2.7.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."js-yaml"."^3.5.2" =
    self.by-version."js-yaml"."3.6.1";
  by-spec."js-yaml"."~3.6.0" =
    self.by-version."js-yaml"."3.6.1";
  by-spec."jsbn"."~0.1.0" =
    self.by-version."jsbn"."0.1.0";
  by-version."jsbn"."0.1.0" = self.buildNodePackage {
    name = "jsbn-0.1.0";
    version = "0.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/jsbn/-/jsbn-0.1.0.tgz";
      name = "jsbn-0.1.0.tgz";
      sha1 = "650987da0dd74f4ebf5a11377a2aa2d273e97dfd";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."jsdom"."^7.0.2" =
    self.by-version."jsdom"."7.2.2";
  by-version."jsdom"."7.2.2" = self.buildNodePackage {
    name = "jsdom-7.2.2";
    version = "7.2.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/jsdom/-/jsdom-7.2.2.tgz";
      name = "jsdom-7.2.2.tgz";
      sha1 = "40b402770c2bda23469096bee91ab675e3b1fc6e";
    };
    deps = {
      "abab-1.0.3" = self.by-version."abab"."1.0.3";
      "acorn-2.7.0" = self.by-version."acorn"."2.7.0";
      "acorn-globals-1.0.9" = self.by-version."acorn-globals"."1.0.9";
      "cssom-0.3.1" = self.by-version."cssom"."0.3.1";
      "cssstyle-0.2.37" = self.by-version."cssstyle"."0.2.37";
      "escodegen-1.8.1" = self.by-version."escodegen"."1.8.1";
      "nwmatcher-1.3.8" = self.by-version."nwmatcher"."1.3.8";
      "parse5-1.5.1" = self.by-version."parse5"."1.5.1";
      "request-2.74.0" = self.by-version."request"."2.74.0";
      "sax-1.2.1" = self.by-version."sax"."1.2.1";
      "symbol-tree-3.1.4" = self.by-version."symbol-tree"."3.1.4";
      "tough-cookie-2.3.1" = self.by-version."tough-cookie"."2.3.1";
      "webidl-conversions-2.0.1" = self.by-version."webidl-conversions"."2.0.1";
      "whatwg-url-compat-0.6.5" = self.by-version."whatwg-url-compat"."0.6.5";
      "xml-name-validator-2.0.1" = self.by-version."xml-name-validator"."2.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."jsesc"."~0.5.0" =
    self.by-version."jsesc"."0.5.0";
  by-version."jsesc"."0.5.0" = self.buildNodePackage {
    name = "jsesc-0.5.0";
    version = "0.5.0";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/jsesc/-/jsesc-0.5.0.tgz";
      name = "jsesc-0.5.0.tgz";
      sha1 = "e7dee66e35d6fc16f710fe91d5cf69f70f08911d";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."json-schema"."0.2.2" =
    self.by-version."json-schema"."0.2.2";
  by-version."json-schema"."0.2.2" = self.buildNodePackage {
    name = "json-schema-0.2.2";
    version = "0.2.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/json-schema/-/json-schema-0.2.2.tgz";
      name = "json-schema-0.2.2.tgz";
      sha1 = "50354f19f603917c695f70b85afa77c3b0f23506";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."json-stable-stringify"."^1.0.0" =
    self.by-version."json-stable-stringify"."1.0.1";
  by-version."json-stable-stringify"."1.0.1" = self.buildNodePackage {
    name = "json-stable-stringify-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/json-stable-stringify/-/json-stable-stringify-1.0.1.tgz";
      name = "json-stable-stringify-1.0.1.tgz";
      sha1 = "9a759d39c5f2ff503fd5300646ed445f88c4f9af";
    };
    deps = {
      "jsonify-0.0.0" = self.by-version."jsonify"."0.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."json-stringify-safe"."~5.0.0" =
    self.by-version."json-stringify-safe"."5.0.1";
  by-version."json-stringify-safe"."5.0.1" = self.buildNodePackage {
    name = "json-stringify-safe-5.0.1";
    version = "5.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/json-stringify-safe/-/json-stringify-safe-5.0.1.tgz";
      name = "json-stringify-safe-5.0.1.tgz";
      sha1 = "1296a2d58fd45f19a0f6ce01d65701e2c735b6eb";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."json-stringify-safe"."~5.0.1" =
    self.by-version."json-stringify-safe"."5.0.1";
  by-spec."json3"."3.2.6" =
    self.by-version."json3"."3.2.6";
  by-version."json3"."3.2.6" = self.buildNodePackage {
    name = "json3-3.2.6";
    version = "3.2.6";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/json3/-/json3-3.2.6.tgz";
      name = "json3-3.2.6.tgz";
      sha1 = "f6efc93c06a04de9aec53053df2559bb19e2038b";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."json3"."3.3.2" =
    self.by-version."json3"."3.3.2";
  by-version."json3"."3.3.2" = self.buildNodePackage {
    name = "json3-3.3.2";
    version = "3.3.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/json3/-/json3-3.3.2.tgz";
      name = "json3-3.3.2.tgz";
      sha1 = "3c0434743df93e2f5c42aee7b19bcb483575f4e1";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."json5"."^0.4.0" =
    self.by-version."json5"."0.4.0";
  by-version."json5"."0.4.0" = self.buildNodePackage {
    name = "json5-0.4.0";
    version = "0.4.0";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/json5/-/json5-0.4.0.tgz";
      name = "json5-0.4.0.tgz";
      sha1 = "054352e4c4c80c86c0923877d449de176a732c8d";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."jsonfile"."^2.1.0" =
    self.by-version."jsonfile"."2.3.1";
  by-version."jsonfile"."2.3.1" = self.buildNodePackage {
    name = "jsonfile-2.3.1";
    version = "2.3.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/jsonfile/-/jsonfile-2.3.1.tgz";
      name = "jsonfile-2.3.1.tgz";
      sha1 = "28bcb29c596b5b7aafd34e662a329ba62cd842fc";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."jsonify"."~0.0.0" =
    self.by-version."jsonify"."0.0.0";
  by-version."jsonify"."0.0.0" = self.buildNodePackage {
    name = "jsonify-0.0.0";
    version = "0.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/jsonify/-/jsonify-0.0.0.tgz";
      name = "jsonify-0.0.0.tgz";
      sha1 = "2c74b6ee41d93ca51b7b5aaee8f503631d252a73";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."jsonpointer"."2.0.0" =
    self.by-version."jsonpointer"."2.0.0";
  by-version."jsonpointer"."2.0.0" = self.buildNodePackage {
    name = "jsonpointer-2.0.0";
    version = "2.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/jsonpointer/-/jsonpointer-2.0.0.tgz";
      name = "jsonpointer-2.0.0.tgz";
      sha1 = "3af1dd20fe85463910d469a385e33017d2a030d9";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."jsprim"."^1.2.2" =
    self.by-version."jsprim"."1.3.0";
  by-version."jsprim"."1.3.0" = self.buildNodePackage {
    name = "jsprim-1.3.0";
    version = "1.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/jsprim/-/jsprim-1.3.0.tgz";
      name = "jsprim-1.3.0.tgz";
      sha1 = "ce2e1bef835204b4f3099928c602f8b6ae615650";
    };
    deps = {
      "extsprintf-1.0.2" = self.by-version."extsprintf"."1.0.2";
      "json-schema-0.2.2" = self.by-version."json-schema"."0.2.2";
      "verror-1.3.6" = self.by-version."verror"."1.3.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."kind-of"."^3.0.2" =
    self.by-version."kind-of"."3.0.4";
  by-version."kind-of"."3.0.4" = self.buildNodePackage {
    name = "kind-of-3.0.4";
    version = "3.0.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/kind-of/-/kind-of-3.0.4.tgz";
      name = "kind-of-3.0.4.tgz";
      sha1 = "7b8ecf18a4e17f8269d73b501c9f232c96887a74";
    };
    deps = {
      "is-buffer-1.1.4" = self.by-version."is-buffer"."1.1.4";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."klaw"."^1.0.0" =
    self.by-version."klaw"."1.3.0";
  by-version."klaw"."1.3.0" = self.buildNodePackage {
    name = "klaw-1.3.0";
    version = "1.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/klaw/-/klaw-1.3.0.tgz";
      name = "klaw-1.3.0.tgz";
      sha1 = "8857bfbc1d824badf13d3d0241d8bbe46fb12f73";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lazy-cache"."^0.2.3" =
    self.by-version."lazy-cache"."0.2.7";
  by-version."lazy-cache"."0.2.7" = self.buildNodePackage {
    name = "lazy-cache-0.2.7";
    version = "0.2.7";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lazy-cache/-/lazy-cache-0.2.7.tgz";
      name = "lazy-cache-0.2.7.tgz";
      sha1 = "7feddf2dcb6edb77d11ef1d117ab5ffdf0ab1b65";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lazy-cache"."^1.0.3" =
    self.by-version."lazy-cache"."1.0.4";
  by-version."lazy-cache"."1.0.4" = self.buildNodePackage {
    name = "lazy-cache-1.0.4";
    version = "1.0.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lazy-cache/-/lazy-cache-1.0.4.tgz";
      name = "lazy-cache-1.0.4.tgz";
      sha1 = "a1d78fc3a50474cb80845d3b3b6e1da49a446e8e";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lazy-req"."^1.0.0" =
    self.by-version."lazy-req"."1.1.0";
  by-version."lazy-req"."1.1.0" = self.buildNodePackage {
    name = "lazy-req-1.1.0";
    version = "1.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lazy-req/-/lazy-req-1.1.0.tgz";
      name = "lazy-req-1.1.0.tgz";
      sha1 = "bdaebead30f8d824039ce0ce149d4daa07ba1fac";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lazypipe"."~1.0.1" =
    self.by-version."lazypipe"."1.0.1";
  by-version."lazypipe"."1.0.1" = self.buildNodePackage {
    name = "lazypipe-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lazypipe/-/lazypipe-1.0.1.tgz";
      name = "lazypipe-1.0.1.tgz";
      sha1 = "1471aef6b37a340d51c34df44699dcef064c1940";
    };
    deps = {
      "stream-combiner-0.2.2" = self.by-version."stream-combiner"."0.2.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "lazypipe" = self.by-version."lazypipe"."1.0.1";
  by-spec."lazystream"."^1.0.0" =
    self.by-version."lazystream"."1.0.0";
  by-version."lazystream"."1.0.0" = self.buildNodePackage {
    name = "lazystream-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lazystream/-/lazystream-1.0.0.tgz";
      name = "lazystream-1.0.0.tgz";
      sha1 = "f6995fe0f820392f61396be89462407bb77168e4";
    };
    deps = {
      "readable-stream-2.1.5" = self.by-version."readable-stream"."2.1.5";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lcid"."^1.0.0" =
    self.by-version."lcid"."1.0.0";
  by-version."lcid"."1.0.0" = self.buildNodePackage {
    name = "lcid-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lcid/-/lcid-1.0.0.tgz";
      name = "lcid-1.0.0.tgz";
      sha1 = "308accafa0bc483a3867b4b6f2b9506251d1b835";
    };
    deps = {
      "invert-kv-1.0.0" = self.by-version."invert-kv"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."ldjson-stream"."^1.1.0" =
    self.by-version."ldjson-stream"."1.2.1";
  by-version."ldjson-stream"."1.2.1" = self.buildNodePackage {
    name = "ldjson-stream-1.2.1";
    version = "1.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/ldjson-stream/-/ldjson-stream-1.2.1.tgz";
      name = "ldjson-stream-1.2.1.tgz";
      sha1 = "91beceda5ac4ed2b17e649fb777e7abfa0189c2b";
    };
    deps = {
      "split2-0.2.1" = self.by-version."split2"."0.2.1";
      "through2-0.6.5" = self.by-version."through2"."0.6.5";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."levn"."~0.3.0" =
    self.by-version."levn"."0.3.0";
  by-version."levn"."0.3.0" = self.buildNodePackage {
    name = "levn-0.3.0";
    version = "0.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/levn/-/levn-0.3.0.tgz";
      name = "levn-0.3.0.tgz";
      sha1 = "3b09924edf9f083c0490fdd4c0bc4421e04764ee";
    };
    deps = {
      "prelude-ls-1.1.2" = self.by-version."prelude-ls"."1.1.2";
      "type-check-0.3.2" = self.by-version."type-check"."0.3.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."liftoff"."^2.1.0" =
    self.by-version."liftoff"."2.3.0";
  by-version."liftoff"."2.3.0" = self.buildNodePackage {
    name = "liftoff-2.3.0";
    version = "2.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/liftoff/-/liftoff-2.3.0.tgz";
      name = "liftoff-2.3.0.tgz";
      sha1 = "a98f2ff67183d8ba7cfaca10548bd7ff0550b385";
    };
    deps = {
      "extend-3.0.0" = self.by-version."extend"."3.0.0";
      "findup-sync-0.4.2" = self.by-version."findup-sync"."0.4.2";
      "fined-1.0.1" = self.by-version."fined"."1.0.1";
      "flagged-respawn-0.3.2" = self.by-version."flagged-respawn"."0.3.2";
      "lodash.isplainobject-4.0.6" = self.by-version."lodash.isplainobject"."4.0.6";
      "lodash.isstring-4.0.1" = self.by-version."lodash.isstring"."4.0.1";
      "lodash.mapvalues-4.6.0" = self.by-version."lodash.mapvalues"."4.6.0";
      "rechoir-0.6.2" = self.by-version."rechoir"."0.6.2";
      "resolve-1.1.7" = self.by-version."resolve"."1.1.7";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."limiter"."^1.0.5" =
    self.by-version."limiter"."1.1.0";
  by-version."limiter"."1.1.0" = self.buildNodePackage {
    name = "limiter-1.1.0";
    version = "1.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/limiter/-/limiter-1.1.0.tgz";
      name = "limiter-1.1.0.tgz";
      sha1 = "6e2bd12ca3fcdaa11f224e2e53c896df3f08d913";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."list-stylesheets"."^1.1.0" =
    self.by-version."list-stylesheets"."1.1.0";
  by-version."list-stylesheets"."1.1.0" = self.buildNodePackage {
    name = "list-stylesheets-1.1.0";
    version = "1.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/list-stylesheets/-/list-stylesheets-1.1.0.tgz";
      name = "list-stylesheets-1.1.0.tgz";
      sha1 = "f0dff0fb187aa2feca9998045994141cc29dbf31";
    };
    deps = {
      "cheerio-0.19.0" = self.by-version."cheerio"."0.19.0";
      "extend-3.0.0" = self.by-version."extend"."3.0.0";
      "object.pick-1.1.2" = self.by-version."object.pick"."1.1.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."litmus-api"."~0.3.2" =
    self.by-version."litmus-api"."0.3.2";
  by-version."litmus-api"."0.3.2" = self.buildNodePackage {
    name = "litmus-api-0.3.2";
    version = "0.3.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/litmus-api/-/litmus-api-0.3.2.tgz";
      name = "litmus-api-0.3.2.tgz";
      sha1 = "71ed2aa713bb880bebad6673d438e52be33afcc8";
    };
    deps = {
      "request-2.36.0" = self.by-version."request"."2.36.0";
      "bluebird-1.2.4" = self.by-version."bluebird"."1.2.4";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."load-json-file"."^1.0.0" =
    self.by-version."load-json-file"."1.1.0";
  by-version."load-json-file"."1.1.0" = self.buildNodePackage {
    name = "load-json-file-1.1.0";
    version = "1.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/load-json-file/-/load-json-file-1.1.0.tgz";
      name = "load-json-file-1.1.0.tgz";
      sha1 = "956905708d58b4bab4c2261b04f59f31c99374c0";
    };
    deps = {
      "graceful-fs-4.1.5" = self.by-version."graceful-fs"."4.1.5";
      "parse-json-2.2.0" = self.by-version."parse-json"."2.2.0";
      "pify-2.3.0" = self.by-version."pify"."2.3.0";
      "pinkie-promise-2.0.1" = self.by-version."pinkie-promise"."2.0.1";
      "strip-bom-2.0.0" = self.by-version."strip-bom"."2.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."load-json-file"."^1.1.0" =
    self.by-version."load-json-file"."1.1.0";
  by-spec."localtunnel"."^1.7.0" =
    self.by-version."localtunnel"."1.8.1";
  by-version."localtunnel"."1.8.1" = self.buildNodePackage {
    name = "localtunnel-1.8.1";
    version = "1.8.1";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/localtunnel/-/localtunnel-1.8.1.tgz";
      name = "localtunnel-1.8.1.tgz";
      sha1 = "d51b2bb7a7066afb05b57fc9db844015098f2e17";
    };
    deps = {
      "request-2.65.0" = self.by-version."request"."2.65.0";
      "yargs-3.29.0" = self.by-version."yargs"."3.29.0";
      "debug-2.2.0" = self.by-version."debug"."2.2.0";
      "openurl-1.1.0" = self.by-version."openurl"."1.1.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash"."^3.10.0" =
    self.by-version."lodash"."3.10.1";
  by-version."lodash"."3.10.1" = self.buildNodePackage {
    name = "lodash-3.10.1";
    version = "3.10.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash/-/lodash-3.10.1.tgz";
      name = "lodash-3.10.1.tgz";
      sha1 = "5bf45e8e49ba4189e17d482789dfd15bd140b7b6";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash"."^3.10.1" =
    self.by-version."lodash"."3.10.1";
  by-spec."lodash"."^3.2.0" =
    self.by-version."lodash"."3.10.1";
  by-spec."lodash"."^3.9.3" =
    self.by-version."lodash"."3.10.1";
  by-spec."lodash"."^4.0.0" =
    self.by-version."lodash"."4.15.0";
  by-version."lodash"."4.15.0" = self.buildNodePackage {
    name = "lodash-4.15.0";
    version = "4.15.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash/-/lodash-4.15.0.tgz";
      name = "lodash-4.15.0.tgz";
      sha1 = "3162391d8f0140aa22cf8f6b3c34d6b7f63d3aa9";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash"."^4.1.0" =
    self.by-version."lodash"."4.15.0";
  by-spec."lodash"."^4.2.0" =
    self.by-version."lodash"."4.15.0";
  by-spec."lodash"."~1.0.1" =
    self.by-version."lodash"."1.0.2";
  by-version."lodash"."1.0.2" = self.buildNodePackage {
    name = "lodash-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash/-/lodash-1.0.2.tgz";
      name = "lodash-1.0.2.tgz";
      sha1 = "8f57560c83b59fc270bd3d561b690043430e2551";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash"."~2.4.1" =
    self.by-version."lodash"."2.4.2";
  by-version."lodash"."2.4.2" = self.buildNodePackage {
    name = "lodash-2.4.2";
    version = "2.4.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash/-/lodash-2.4.2.tgz";
      name = "lodash-2.4.2.tgz";
      sha1 = "fadd834b9683073da179b3eae6d9c0d15053f73e";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash"."~3.5.0" =
    self.by-version."lodash"."3.5.0";
  by-version."lodash"."3.5.0" = self.buildNodePackage {
    name = "lodash-3.5.0";
    version = "3.5.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash/-/lodash-3.5.0.tgz";
      name = "lodash-3.5.0.tgz";
      sha1 = "19bb3f4d51278f0b8c818ed145c74ecf9fe40e6d";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash"."~4.9.0" =
    self.by-version."lodash"."4.9.0";
  by-version."lodash"."4.9.0" = self.buildNodePackage {
    name = "lodash-4.9.0";
    version = "4.9.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash/-/lodash-4.9.0.tgz";
      name = "lodash-4.9.0.tgz";
      sha1 = "4c20d742f03ce85dc700e0dd7ab9bcab85e6fc14";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash-node"."~2.4.1" =
    self.by-version."lodash-node"."2.4.1";
  by-version."lodash-node"."2.4.1" = self.buildNodePackage {
    name = "lodash-node-2.4.1";
    version = "2.4.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash-node/-/lodash-node-2.4.1.tgz";
      name = "lodash-node-2.4.1.tgz";
      sha1 = "ea82f7b100c733d1a42af76801e506105e2a80ec";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash._arraycopy"."^3.0.0" =
    self.by-version."lodash._arraycopy"."3.0.0";
  by-version."lodash._arraycopy"."3.0.0" = self.buildNodePackage {
    name = "lodash._arraycopy-3.0.0";
    version = "3.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash._arraycopy/-/lodash._arraycopy-3.0.0.tgz";
      name = "lodash._arraycopy-3.0.0.tgz";
      sha1 = "76e7b7c1f1fb92547374878a562ed06a3e50f6e1";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash._arrayeach"."^3.0.0" =
    self.by-version."lodash._arrayeach"."3.0.0";
  by-version."lodash._arrayeach"."3.0.0" = self.buildNodePackage {
    name = "lodash._arrayeach-3.0.0";
    version = "3.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash._arrayeach/-/lodash._arrayeach-3.0.0.tgz";
      name = "lodash._arrayeach-3.0.0.tgz";
      sha1 = "bab156b2a90d3f1bbd5c653403349e5e5933ef9e";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash._baseclone"."^4.0.0" =
    self.by-version."lodash._baseclone"."4.5.7";
  by-version."lodash._baseclone"."4.5.7" = self.buildNodePackage {
    name = "lodash._baseclone-4.5.7";
    version = "4.5.7";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash._baseclone/-/lodash._baseclone-4.5.7.tgz";
      name = "lodash._baseclone-4.5.7.tgz";
      sha1 = "ce42ade08384ef5d62fa77c30f61a46e686f8434";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash._basecopy"."^3.0.0" =
    self.by-version."lodash._basecopy"."3.0.1";
  by-version."lodash._basecopy"."3.0.1" = self.buildNodePackage {
    name = "lodash._basecopy-3.0.1";
    version = "3.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash._basecopy/-/lodash._basecopy-3.0.1.tgz";
      name = "lodash._basecopy-3.0.1.tgz";
      sha1 = "8da0e6a876cf344c0ad8a54882111dd3c5c7ca36";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash._basefor"."^3.0.0" =
    self.by-version."lodash._basefor"."3.0.3";
  by-version."lodash._basefor"."3.0.3" = self.buildNodePackage {
    name = "lodash._basefor-3.0.3";
    version = "3.0.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash._basefor/-/lodash._basefor-3.0.3.tgz";
      name = "lodash._basefor-3.0.3.tgz";
      sha1 = "7550b4e9218ef09fad24343b612021c79b4c20c2";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash._basetostring"."^3.0.0" =
    self.by-version."lodash._basetostring"."3.0.1";
  by-version."lodash._basetostring"."3.0.1" = self.buildNodePackage {
    name = "lodash._basetostring-3.0.1";
    version = "3.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash._basetostring/-/lodash._basetostring-3.0.1.tgz";
      name = "lodash._basetostring-3.0.1.tgz";
      sha1 = "d1861d877f824a52f669832dcaf3ee15566a07d5";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash._basevalues"."^3.0.0" =
    self.by-version."lodash._basevalues"."3.0.0";
  by-version."lodash._basevalues"."3.0.0" = self.buildNodePackage {
    name = "lodash._basevalues-3.0.0";
    version = "3.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash._basevalues/-/lodash._basevalues-3.0.0.tgz";
      name = "lodash._basevalues-3.0.0.tgz";
      sha1 = "5b775762802bde3d3297503e26300820fdf661b7";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash._bindcallback"."^3.0.0" =
    self.by-version."lodash._bindcallback"."3.0.1";
  by-version."lodash._bindcallback"."3.0.1" = self.buildNodePackage {
    name = "lodash._bindcallback-3.0.1";
    version = "3.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash._bindcallback/-/lodash._bindcallback-3.0.1.tgz";
      name = "lodash._bindcallback-3.0.1.tgz";
      sha1 = "e531c27644cf8b57a99e17ed95b35c748789392e";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash._createassigner"."^3.0.0" =
    self.by-version."lodash._createassigner"."3.1.1";
  by-version."lodash._createassigner"."3.1.1" = self.buildNodePackage {
    name = "lodash._createassigner-3.1.1";
    version = "3.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash._createassigner/-/lodash._createassigner-3.1.1.tgz";
      name = "lodash._createassigner-3.1.1.tgz";
      sha1 = "838a5bae2fdaca63ac22dee8e19fa4e6d6970b11";
    };
    deps = {
      "lodash._bindcallback-3.0.1" = self.by-version."lodash._bindcallback"."3.0.1";
      "lodash._isiterateecall-3.0.9" = self.by-version."lodash._isiterateecall"."3.0.9";
      "lodash.restparam-3.6.1" = self.by-version."lodash.restparam"."3.6.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash._escapehtmlchar"."~2.4.1" =
    self.by-version."lodash._escapehtmlchar"."2.4.1";
  by-version."lodash._escapehtmlchar"."2.4.1" = self.buildNodePackage {
    name = "lodash._escapehtmlchar-2.4.1";
    version = "2.4.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash._escapehtmlchar/-/lodash._escapehtmlchar-2.4.1.tgz";
      name = "lodash._escapehtmlchar-2.4.1.tgz";
      sha1 = "df67c3bb6b7e8e1e831ab48bfa0795b92afe899d";
    };
    deps = {
      "lodash._htmlescapes-2.4.1" = self.by-version."lodash._htmlescapes"."2.4.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash._escapestringchar"."~2.4.1" =
    self.by-version."lodash._escapestringchar"."2.4.1";
  by-version."lodash._escapestringchar"."2.4.1" = self.buildNodePackage {
    name = "lodash._escapestringchar-2.4.1";
    version = "2.4.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash._escapestringchar/-/lodash._escapestringchar-2.4.1.tgz";
      name = "lodash._escapestringchar-2.4.1.tgz";
      sha1 = "ecfe22618a2ade50bfeea43937e51df66f0edb72";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash._getnative"."^3.0.0" =
    self.by-version."lodash._getnative"."3.9.1";
  by-version."lodash._getnative"."3.9.1" = self.buildNodePackage {
    name = "lodash._getnative-3.9.1";
    version = "3.9.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash._getnative/-/lodash._getnative-3.9.1.tgz";
      name = "lodash._getnative-3.9.1.tgz";
      sha1 = "570bc7dede46d61cdcde687d65d3eecbaa3aaff5";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash._htmlescapes"."~2.4.1" =
    self.by-version."lodash._htmlescapes"."2.4.1";
  by-version."lodash._htmlescapes"."2.4.1" = self.buildNodePackage {
    name = "lodash._htmlescapes-2.4.1";
    version = "2.4.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash._htmlescapes/-/lodash._htmlescapes-2.4.1.tgz";
      name = "lodash._htmlescapes-2.4.1.tgz";
      sha1 = "32d14bf0844b6de6f8b62a051b4f67c228b624cb";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash._isiterateecall"."^3.0.0" =
    self.by-version."lodash._isiterateecall"."3.0.9";
  by-version."lodash._isiterateecall"."3.0.9" = self.buildNodePackage {
    name = "lodash._isiterateecall-3.0.9";
    version = "3.0.9";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash._isiterateecall/-/lodash._isiterateecall-3.0.9.tgz";
      name = "lodash._isiterateecall-3.0.9.tgz";
      sha1 = "5203ad7ba425fae842460e696db9cf3e6aac057c";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash._isnative"."~2.4.1" =
    self.by-version."lodash._isnative"."2.4.1";
  by-version."lodash._isnative"."2.4.1" = self.buildNodePackage {
    name = "lodash._isnative-2.4.1";
    version = "2.4.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash._isnative/-/lodash._isnative-2.4.1.tgz";
      name = "lodash._isnative-2.4.1.tgz";
      sha1 = "3ea6404b784a7be836c7b57580e1cdf79b14832c";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash._objecttypes"."~2.4.1" =
    self.by-version."lodash._objecttypes"."2.4.1";
  by-version."lodash._objecttypes"."2.4.1" = self.buildNodePackage {
    name = "lodash._objecttypes-2.4.1";
    version = "2.4.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash._objecttypes/-/lodash._objecttypes-2.4.1.tgz";
      name = "lodash._objecttypes-2.4.1.tgz";
      sha1 = "7c0b7f69d98a1f76529f890b0cdb1b4dfec11c11";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash._reescape"."^3.0.0" =
    self.by-version."lodash._reescape"."3.0.0";
  by-version."lodash._reescape"."3.0.0" = self.buildNodePackage {
    name = "lodash._reescape-3.0.0";
    version = "3.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash._reescape/-/lodash._reescape-3.0.0.tgz";
      name = "lodash._reescape-3.0.0.tgz";
      sha1 = "2b1d6f5dfe07c8a355753e5f27fac7f1cde1616a";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash._reevaluate"."^3.0.0" =
    self.by-version."lodash._reevaluate"."3.0.0";
  by-version."lodash._reevaluate"."3.0.0" = self.buildNodePackage {
    name = "lodash._reevaluate-3.0.0";
    version = "3.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash._reevaluate/-/lodash._reevaluate-3.0.0.tgz";
      name = "lodash._reevaluate-3.0.0.tgz";
      sha1 = "58bc74c40664953ae0b124d806996daca431e2ed";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash._reinterpolate"."^2.4.1" =
    self.by-version."lodash._reinterpolate"."2.4.1";
  by-version."lodash._reinterpolate"."2.4.1" = self.buildNodePackage {
    name = "lodash._reinterpolate-2.4.1";
    version = "2.4.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash._reinterpolate/-/lodash._reinterpolate-2.4.1.tgz";
      name = "lodash._reinterpolate-2.4.1.tgz";
      sha1 = "4f1227aa5a8711fc632f5b07a1f4607aab8b3222";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash._reinterpolate"."^3.0.0" =
    self.by-version."lodash._reinterpolate"."3.0.0";
  by-version."lodash._reinterpolate"."3.0.0" = self.buildNodePackage {
    name = "lodash._reinterpolate-3.0.0";
    version = "3.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash._reinterpolate/-/lodash._reinterpolate-3.0.0.tgz";
      name = "lodash._reinterpolate-3.0.0.tgz";
      sha1 = "0ccf2d89166af03b3663c796538b75ac6e114d9d";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash._reinterpolate"."~2.4.1" =
    self.by-version."lodash._reinterpolate"."2.4.1";
  by-spec."lodash._reunescapedhtml"."~2.4.1" =
    self.by-version."lodash._reunescapedhtml"."2.4.1";
  by-version."lodash._reunescapedhtml"."2.4.1" = self.buildNodePackage {
    name = "lodash._reunescapedhtml-2.4.1";
    version = "2.4.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash._reunescapedhtml/-/lodash._reunescapedhtml-2.4.1.tgz";
      name = "lodash._reunescapedhtml-2.4.1.tgz";
      sha1 = "747c4fc40103eb3bb8a0976e571f7a2659e93ba7";
    };
    deps = {
      "lodash._htmlescapes-2.4.1" = self.by-version."lodash._htmlescapes"."2.4.1";
      "lodash.keys-2.4.1" = self.by-version."lodash.keys"."2.4.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash._root"."^3.0.0" =
    self.by-version."lodash._root"."3.0.1";
  by-version."lodash._root"."3.0.1" = self.buildNodePackage {
    name = "lodash._root-3.0.1";
    version = "3.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash._root/-/lodash._root-3.0.1.tgz";
      name = "lodash._root-3.0.1.tgz";
      sha1 = "fba1c4524c19ee9a5f8136b4609f017cf4ded692";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash._shimkeys"."~2.4.1" =
    self.by-version."lodash._shimkeys"."2.4.1";
  by-version."lodash._shimkeys"."2.4.1" = self.buildNodePackage {
    name = "lodash._shimkeys-2.4.1";
    version = "2.4.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash._shimkeys/-/lodash._shimkeys-2.4.1.tgz";
      name = "lodash._shimkeys-2.4.1.tgz";
      sha1 = "6e9cc9666ff081f0b5a6c978b83e242e6949d203";
    };
    deps = {
      "lodash._objecttypes-2.4.1" = self.by-version."lodash._objecttypes"."2.4.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash.assign"."^4.0.3" =
    self.by-version."lodash.assign"."4.2.0";
  by-version."lodash.assign"."4.2.0" = self.buildNodePackage {
    name = "lodash.assign-4.2.0";
    version = "4.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash.assign/-/lodash.assign-4.2.0.tgz";
      name = "lodash.assign-4.2.0.tgz";
      sha1 = "0d99f3ccd7a6d261d19bdaeb9245005d285808e7";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash.assign"."^4.0.6" =
    self.by-version."lodash.assign"."4.2.0";
  by-spec."lodash.assignwith"."^4.0.7" =
    self.by-version."lodash.assignwith"."4.2.0";
  by-version."lodash.assignwith"."4.2.0" = self.buildNodePackage {
    name = "lodash.assignwith-4.2.0";
    version = "4.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash.assignwith/-/lodash.assignwith-4.2.0.tgz";
      name = "lodash.assignwith-4.2.0.tgz";
      sha1 = "127a97f02adc41751a954d24b0de17e100e038eb";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash.clonedeep"."4.3.1" =
    self.by-version."lodash.clonedeep"."4.3.1";
  by-version."lodash.clonedeep"."4.3.1" = self.buildNodePackage {
    name = "lodash.clonedeep-4.3.1";
    version = "4.3.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash.clonedeep/-/lodash.clonedeep-4.3.1.tgz";
      name = "lodash.clonedeep-4.3.1.tgz";
      sha1 = "94bd4e5267be2f72f567aa0b7b650c5044e24e71";
    };
    deps = {
      "lodash._baseclone-4.5.7" = self.by-version."lodash._baseclone"."4.5.7";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash.clonedeep"."^4.3.2" =
    self.by-version."lodash.clonedeep"."4.5.0";
  by-version."lodash.clonedeep"."4.5.0" = self.buildNodePackage {
    name = "lodash.clonedeep-4.5.0";
    version = "4.5.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash.clonedeep/-/lodash.clonedeep-4.5.0.tgz";
      name = "lodash.clonedeep-4.5.0.tgz";
      sha1 = "e23f3f9c4f8fbdde872529c1071857a086e5ccef";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash.defaults"."~2.4.1" =
    self.by-version."lodash.defaults"."2.4.1";
  by-version."lodash.defaults"."2.4.1" = self.buildNodePackage {
    name = "lodash.defaults-2.4.1";
    version = "2.4.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash.defaults/-/lodash.defaults-2.4.1.tgz";
      name = "lodash.defaults-2.4.1.tgz";
      sha1 = "a7e8885f05e68851144b6e12a8f3678026bc4c54";
    };
    deps = {
      "lodash.keys-2.4.1" = self.by-version."lodash.keys"."2.4.1";
      "lodash._objecttypes-2.4.1" = self.by-version."lodash._objecttypes"."2.4.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash.escape"."^3.0.0" =
    self.by-version."lodash.escape"."3.2.0";
  by-version."lodash.escape"."3.2.0" = self.buildNodePackage {
    name = "lodash.escape-3.2.0";
    version = "3.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash.escape/-/lodash.escape-3.2.0.tgz";
      name = "lodash.escape-3.2.0.tgz";
      sha1 = "995ee0dc18c1b48cc92effae71a10aab5b487698";
    };
    deps = {
      "lodash._root-3.0.1" = self.by-version."lodash._root"."3.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash.escape"."~2.4.1" =
    self.by-version."lodash.escape"."2.4.1";
  by-version."lodash.escape"."2.4.1" = self.buildNodePackage {
    name = "lodash.escape-2.4.1";
    version = "2.4.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash.escape/-/lodash.escape-2.4.1.tgz";
      name = "lodash.escape-2.4.1.tgz";
      sha1 = "2ce12c5e084db0a57dda5e5d1eeeb9f5d175a3b4";
    };
    deps = {
      "lodash._escapehtmlchar-2.4.1" = self.by-version."lodash._escapehtmlchar"."2.4.1";
      "lodash.keys-2.4.1" = self.by-version."lodash.keys"."2.4.1";
      "lodash._reunescapedhtml-2.4.1" = self.by-version."lodash._reunescapedhtml"."2.4.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash.isarguments"."^3.0.0" =
    self.by-version."lodash.isarguments"."3.1.0";
  by-version."lodash.isarguments"."3.1.0" = self.buildNodePackage {
    name = "lodash.isarguments-3.1.0";
    version = "3.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash.isarguments/-/lodash.isarguments-3.1.0.tgz";
      name = "lodash.isarguments-3.1.0.tgz";
      sha1 = "2f573d85c6a24289ff00663b491c1d338ff3458a";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash.isarray"."^3.0.0" =
    self.by-version."lodash.isarray"."3.0.4";
  by-version."lodash.isarray"."3.0.4" = self.buildNodePackage {
    name = "lodash.isarray-3.0.4";
    version = "3.0.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash.isarray/-/lodash.isarray-3.0.4.tgz";
      name = "lodash.isarray-3.0.4.tgz";
      sha1 = "79e4eb88c36a8122af86f844aa9bcd851b5fbb55";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash.isarray"."^4.0.0" =
    self.by-version."lodash.isarray"."4.0.0";
  by-version."lodash.isarray"."4.0.0" = self.buildNodePackage {
    name = "lodash.isarray-4.0.0";
    version = "4.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash.isarray/-/lodash.isarray-4.0.0.tgz";
      name = "lodash.isarray-4.0.0.tgz";
      sha1 = "2aca496b28c4ca6d726715313590c02e6ea34403";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash.isempty"."^4.2.1" =
    self.by-version."lodash.isempty"."4.4.0";
  by-version."lodash.isempty"."4.4.0" = self.buildNodePackage {
    name = "lodash.isempty-4.4.0";
    version = "4.4.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash.isempty/-/lodash.isempty-4.4.0.tgz";
      name = "lodash.isempty-4.4.0.tgz";
      sha1 = "6f86cbedd8be4ec987be9aaf33c9684db1b31e7e";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash.isequal"."^4.0.0" =
    self.by-version."lodash.isequal"."4.4.0";
  by-version."lodash.isequal"."4.4.0" = self.buildNodePackage {
    name = "lodash.isequal-4.4.0";
    version = "4.4.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash.isequal/-/lodash.isequal-4.4.0.tgz";
      name = "lodash.isequal-4.4.0.tgz";
      sha1 = "6295768e98e14dc15ce8d362ef6340db82852031";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash.isobject"."~2.4.1" =
    self.by-version."lodash.isobject"."2.4.1";
  by-version."lodash.isobject"."2.4.1" = self.buildNodePackage {
    name = "lodash.isobject-2.4.1";
    version = "2.4.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash.isobject/-/lodash.isobject-2.4.1.tgz";
      name = "lodash.isobject-2.4.1.tgz";
      sha1 = "5a2e47fe69953f1ee631a7eba1fe64d2d06558f5";
    };
    deps = {
      "lodash._objecttypes-2.4.1" = self.by-version."lodash._objecttypes"."2.4.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash.isplainobject"."^3.0.0" =
    self.by-version."lodash.isplainobject"."3.2.0";
  by-version."lodash.isplainobject"."3.2.0" = self.buildNodePackage {
    name = "lodash.isplainobject-3.2.0";
    version = "3.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash.isplainobject/-/lodash.isplainobject-3.2.0.tgz";
      name = "lodash.isplainobject-3.2.0.tgz";
      sha1 = "9a8238ae16b200432960cd7346512d0123fbf4c5";
    };
    deps = {
      "lodash._basefor-3.0.3" = self.by-version."lodash._basefor"."3.0.3";
      "lodash.isarguments-3.1.0" = self.by-version."lodash.isarguments"."3.1.0";
      "lodash.keysin-3.0.8" = self.by-version."lodash.keysin"."3.0.8";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash.isplainobject"."^4.0.4" =
    self.by-version."lodash.isplainobject"."4.0.6";
  by-version."lodash.isplainobject"."4.0.6" = self.buildNodePackage {
    name = "lodash.isplainobject-4.0.6";
    version = "4.0.6";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash.isplainobject/-/lodash.isplainobject-4.0.6.tgz";
      name = "lodash.isplainobject-4.0.6.tgz";
      sha1 = "7c526a52d89b45c45cc690b88163be0497f550cb";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash.isstring"."^4.0.1" =
    self.by-version."lodash.isstring"."4.0.1";
  by-version."lodash.isstring"."4.0.1" = self.buildNodePackage {
    name = "lodash.isstring-4.0.1";
    version = "4.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash.isstring/-/lodash.isstring-4.0.1.tgz";
      name = "lodash.isstring-4.0.1.tgz";
      sha1 = "d527dfb5456eca7cc9bb95d5daeaf88ba54a5451";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash.istypedarray"."^3.0.0" =
    self.by-version."lodash.istypedarray"."3.0.6";
  by-version."lodash.istypedarray"."3.0.6" = self.buildNodePackage {
    name = "lodash.istypedarray-3.0.6";
    version = "3.0.6";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash.istypedarray/-/lodash.istypedarray-3.0.6.tgz";
      name = "lodash.istypedarray-3.0.6.tgz";
      sha1 = "c9a477498607501d8e8494d283b87c39281cef62";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash.keys"."^3.0.0" =
    self.by-version."lodash.keys"."3.1.2";
  by-version."lodash.keys"."3.1.2" = self.buildNodePackage {
    name = "lodash.keys-3.1.2";
    version = "3.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash.keys/-/lodash.keys-3.1.2.tgz";
      name = "lodash.keys-3.1.2.tgz";
      sha1 = "4dbc0472b156be50a0b286855d1bd0b0c656098a";
    };
    deps = {
      "lodash._getnative-3.9.1" = self.by-version."lodash._getnative"."3.9.1";
      "lodash.isarguments-3.1.0" = self.by-version."lodash.isarguments"."3.1.0";
      "lodash.isarray-3.0.4" = self.by-version."lodash.isarray"."3.0.4";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash.keys"."~2.4.1" =
    self.by-version."lodash.keys"."2.4.1";
  by-version."lodash.keys"."2.4.1" = self.buildNodePackage {
    name = "lodash.keys-2.4.1";
    version = "2.4.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash.keys/-/lodash.keys-2.4.1.tgz";
      name = "lodash.keys-2.4.1.tgz";
      sha1 = "48dea46df8ff7632b10d706b8acb26591e2b3727";
    };
    deps = {
      "lodash._isnative-2.4.1" = self.by-version."lodash._isnative"."2.4.1";
      "lodash.isobject-2.4.1" = self.by-version."lodash.isobject"."2.4.1";
      "lodash._shimkeys-2.4.1" = self.by-version."lodash._shimkeys"."2.4.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash.keysin"."^3.0.0" =
    self.by-version."lodash.keysin"."3.0.8";
  by-version."lodash.keysin"."3.0.8" = self.buildNodePackage {
    name = "lodash.keysin-3.0.8";
    version = "3.0.8";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash.keysin/-/lodash.keysin-3.0.8.tgz";
      name = "lodash.keysin-3.0.8.tgz";
      sha1 = "22c4493ebbedb1427962a54b445b2c8a767fb47f";
    };
    deps = {
      "lodash.isarguments-3.1.0" = self.by-version."lodash.isarguments"."3.1.0";
      "lodash.isarray-3.0.4" = self.by-version."lodash.isarray"."3.0.4";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash.mapvalues"."^4.4.0" =
    self.by-version."lodash.mapvalues"."4.6.0";
  by-version."lodash.mapvalues"."4.6.0" = self.buildNodePackage {
    name = "lodash.mapvalues-4.6.0";
    version = "4.6.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash.mapvalues/-/lodash.mapvalues-4.6.0.tgz";
      name = "lodash.mapvalues-4.6.0.tgz";
      sha1 = "1bafa5005de9dd6f4f26668c30ca37230cc9689c";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash.merge"."^3.3.1" =
    self.by-version."lodash.merge"."3.3.2";
  by-version."lodash.merge"."3.3.2" = self.buildNodePackage {
    name = "lodash.merge-3.3.2";
    version = "3.3.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash.merge/-/lodash.merge-3.3.2.tgz";
      name = "lodash.merge-3.3.2.tgz";
      sha1 = "0d90d93ed637b1878437bb3e21601260d7afe994";
    };
    deps = {
      "lodash._arraycopy-3.0.0" = self.by-version."lodash._arraycopy"."3.0.0";
      "lodash._arrayeach-3.0.0" = self.by-version."lodash._arrayeach"."3.0.0";
      "lodash._createassigner-3.1.1" = self.by-version."lodash._createassigner"."3.1.1";
      "lodash._getnative-3.9.1" = self.by-version."lodash._getnative"."3.9.1";
      "lodash.isarguments-3.1.0" = self.by-version."lodash.isarguments"."3.1.0";
      "lodash.isarray-3.0.4" = self.by-version."lodash.isarray"."3.0.4";
      "lodash.isplainobject-3.2.0" = self.by-version."lodash.isplainobject"."3.2.0";
      "lodash.istypedarray-3.0.6" = self.by-version."lodash.istypedarray"."3.0.6";
      "lodash.keys-3.1.2" = self.by-version."lodash.keys"."3.1.2";
      "lodash.keysin-3.0.8" = self.by-version."lodash.keysin"."3.0.8";
      "lodash.toplainobject-3.0.0" = self.by-version."lodash.toplainobject"."3.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash.pick"."^4.2.1" =
    self.by-version."lodash.pick"."4.4.0";
  by-version."lodash.pick"."4.4.0" = self.buildNodePackage {
    name = "lodash.pick-4.4.0";
    version = "4.4.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash.pick/-/lodash.pick-4.4.0.tgz";
      name = "lodash.pick-4.4.0.tgz";
      sha1 = "52f05610fff9ded422611441ed1fc123a03001b3";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash.restparam"."^3.0.0" =
    self.by-version."lodash.restparam"."3.6.1";
  by-version."lodash.restparam"."3.6.1" = self.buildNodePackage {
    name = "lodash.restparam-3.6.1";
    version = "3.6.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash.restparam/-/lodash.restparam-3.6.1.tgz";
      name = "lodash.restparam-3.6.1.tgz";
      sha1 = "936a4e309ef330a7645ed4145986c85ae5b20805";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash.template"."^2.4.1" =
    self.by-version."lodash.template"."2.4.1";
  by-version."lodash.template"."2.4.1" = self.buildNodePackage {
    name = "lodash.template-2.4.1";
    version = "2.4.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash.template/-/lodash.template-2.4.1.tgz";
      name = "lodash.template-2.4.1.tgz";
      sha1 = "9e611007edf629129a974ab3c48b817b3e1cf20d";
    };
    deps = {
      "lodash.defaults-2.4.1" = self.by-version."lodash.defaults"."2.4.1";
      "lodash.escape-2.4.1" = self.by-version."lodash.escape"."2.4.1";
      "lodash._escapestringchar-2.4.1" = self.by-version."lodash._escapestringchar"."2.4.1";
      "lodash.keys-2.4.1" = self.by-version."lodash.keys"."2.4.1";
      "lodash._reinterpolate-2.4.1" = self.by-version."lodash._reinterpolate"."2.4.1";
      "lodash.templatesettings-2.4.1" = self.by-version."lodash.templatesettings"."2.4.1";
      "lodash.values-2.4.1" = self.by-version."lodash.values"."2.4.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash.template"."^3.0.0" =
    self.by-version."lodash.template"."3.6.2";
  by-version."lodash.template"."3.6.2" = self.buildNodePackage {
    name = "lodash.template-3.6.2";
    version = "3.6.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash.template/-/lodash.template-3.6.2.tgz";
      name = "lodash.template-3.6.2.tgz";
      sha1 = "f8cdecc6169a255be9098ae8b0c53d378931d14f";
    };
    deps = {
      "lodash._basecopy-3.0.1" = self.by-version."lodash._basecopy"."3.0.1";
      "lodash._basetostring-3.0.1" = self.by-version."lodash._basetostring"."3.0.1";
      "lodash._basevalues-3.0.0" = self.by-version."lodash._basevalues"."3.0.0";
      "lodash._isiterateecall-3.0.9" = self.by-version."lodash._isiterateecall"."3.0.9";
      "lodash._reinterpolate-3.0.0" = self.by-version."lodash._reinterpolate"."3.0.0";
      "lodash.escape-3.2.0" = self.by-version."lodash.escape"."3.2.0";
      "lodash.keys-3.1.2" = self.by-version."lodash.keys"."3.1.2";
      "lodash.restparam-3.6.1" = self.by-version."lodash.restparam"."3.6.1";
      "lodash.templatesettings-3.1.1" = self.by-version."lodash.templatesettings"."3.1.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash.templatesettings"."^3.0.0" =
    self.by-version."lodash.templatesettings"."3.1.1";
  by-version."lodash.templatesettings"."3.1.1" = self.buildNodePackage {
    name = "lodash.templatesettings-3.1.1";
    version = "3.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash.templatesettings/-/lodash.templatesettings-3.1.1.tgz";
      name = "lodash.templatesettings-3.1.1.tgz";
      sha1 = "fb307844753b66b9f1afa54e262c745307dba8e5";
    };
    deps = {
      "lodash._reinterpolate-3.0.0" = self.by-version."lodash._reinterpolate"."3.0.0";
      "lodash.escape-3.2.0" = self.by-version."lodash.escape"."3.2.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash.templatesettings"."~2.4.1" =
    self.by-version."lodash.templatesettings"."2.4.1";
  by-version."lodash.templatesettings"."2.4.1" = self.buildNodePackage {
    name = "lodash.templatesettings-2.4.1";
    version = "2.4.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash.templatesettings/-/lodash.templatesettings-2.4.1.tgz";
      name = "lodash.templatesettings-2.4.1.tgz";
      sha1 = "ea76c75d11eb86d4dbe89a83893bb861929ac699";
    };
    deps = {
      "lodash.escape-2.4.1" = self.by-version."lodash.escape"."2.4.1";
      "lodash._reinterpolate-2.4.1" = self.by-version."lodash._reinterpolate"."2.4.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash.toplainobject"."^3.0.0" =
    self.by-version."lodash.toplainobject"."3.0.0";
  by-version."lodash.toplainobject"."3.0.0" = self.buildNodePackage {
    name = "lodash.toplainobject-3.0.0";
    version = "3.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash.toplainobject/-/lodash.toplainobject-3.0.0.tgz";
      name = "lodash.toplainobject-3.0.0.tgz";
      sha1 = "28790ad942d293d78aa663a07ecf7f52ca04198d";
    };
    deps = {
      "lodash._basecopy-3.0.1" = self.by-version."lodash._basecopy"."3.0.1";
      "lodash.keysin-3.0.8" = self.by-version."lodash.keysin"."3.0.8";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lodash.values"."~2.4.1" =
    self.by-version."lodash.values"."2.4.1";
  by-version."lodash.values"."2.4.1" = self.buildNodePackage {
    name = "lodash.values-2.4.1";
    version = "2.4.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lodash.values/-/lodash.values-2.4.1.tgz";
      name = "lodash.values-2.4.1.tgz";
      sha1 = "abf514436b3cb705001627978cbcf30b1280eea4";
    };
    deps = {
      "lodash.keys-2.4.1" = self.by-version."lodash.keys"."2.4.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."logalot"."^2.0.0" =
    self.by-version."logalot"."2.1.0";
  by-version."logalot"."2.1.0" = self.buildNodePackage {
    name = "logalot-2.1.0";
    version = "2.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/logalot/-/logalot-2.1.0.tgz";
      name = "logalot-2.1.0.tgz";
      sha1 = "5f8e8c90d304edf12530951a5554abb8c5e3f552";
    };
    deps = {
      "figures-1.7.0" = self.by-version."figures"."1.7.0";
      "squeak-1.3.0" = self.by-version."squeak"."1.3.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."longest"."^1.0.0" =
    self.by-version."longest"."1.0.1";
  by-version."longest"."1.0.1" = self.buildNodePackage {
    name = "longest-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/longest/-/longest-1.0.1.tgz";
      name = "longest-1.0.1.tgz";
      sha1 = "30a0b2da38f73770e8294a0d22e6625ed77d0097";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."longest"."^1.0.1" =
    self.by-version."longest"."1.0.1";
  by-spec."loose-envify"."^1.0.0" =
    self.by-version."loose-envify"."1.2.0";
  by-version."loose-envify"."1.2.0" = self.buildNodePackage {
    name = "loose-envify-1.2.0";
    version = "1.2.0";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/loose-envify/-/loose-envify-1.2.0.tgz";
      name = "loose-envify-1.2.0.tgz";
      sha1 = "69a65aad3de542cf4ee0f4fe74e8e33c709ccb0f";
    };
    deps = {
      "js-tokens-1.0.3" = self.by-version."js-tokens"."1.0.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."loud-rejection"."^1.0.0" =
    self.by-version."loud-rejection"."1.6.0";
  by-version."loud-rejection"."1.6.0" = self.buildNodePackage {
    name = "loud-rejection-1.6.0";
    version = "1.6.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/loud-rejection/-/loud-rejection-1.6.0.tgz";
      name = "loud-rejection-1.6.0.tgz";
      sha1 = "5b46f80147edee578870f086d04821cf998e551f";
    };
    deps = {
      "currently-unhandled-0.4.1" = self.by-version."currently-unhandled"."0.4.1";
      "signal-exit-3.0.0" = self.by-version."signal-exit"."3.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lower-case"."^1.1.0" =
    self.by-version."lower-case"."1.1.3";
  by-version."lower-case"."1.1.3" = self.buildNodePackage {
    name = "lower-case-1.1.3";
    version = "1.1.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lower-case/-/lower-case-1.1.3.tgz";
      name = "lower-case-1.1.3.tgz";
      sha1 = "c92393d976793eee5ba4edb583cf8eae35bd9bfb";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lower-case"."^1.1.1" =
    self.by-version."lower-case"."1.1.3";
  by-spec."lower-case"."^1.1.2" =
    self.by-version."lower-case"."1.1.3";
  by-spec."lower-case-first"."^1.0.0" =
    self.by-version."lower-case-first"."1.0.2";
  by-version."lower-case-first"."1.0.2" = self.buildNodePackage {
    name = "lower-case-first-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lower-case-first/-/lower-case-first-1.0.2.tgz";
      name = "lower-case-first-1.0.2.tgz";
      sha1 = "e5da7c26f29a7073be02d52bac9980e5922adfa1";
    };
    deps = {
      "lower-case-1.1.3" = self.by-version."lower-case"."1.1.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lowercase-keys"."^1.0.0" =
    self.by-version."lowercase-keys"."1.0.0";
  by-version."lowercase-keys"."1.0.0" = self.buildNodePackage {
    name = "lowercase-keys-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lowercase-keys/-/lowercase-keys-1.0.0.tgz";
      name = "lowercase-keys-1.0.0.tgz";
      sha1 = "4e3366b39e7f5457e35f1324bdf6f88d0bfc7306";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lpad"."^2.0.1" =
    self.by-version."lpad"."2.0.1";
  by-version."lpad"."2.0.1" = self.buildNodePackage {
    name = "lpad-2.0.1";
    version = "2.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lpad/-/lpad-2.0.1.tgz";
      name = "lpad-2.0.1.tgz";
      sha1 = "28316b4e7b2015f511f6591459afc0e5944008ad";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lpad-align"."^1.0.1" =
    self.by-version."lpad-align"."1.1.0";
  by-version."lpad-align"."1.1.0" = self.buildNodePackage {
    name = "lpad-align-1.1.0";
    version = "1.1.0";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/lpad-align/-/lpad-align-1.1.0.tgz";
      name = "lpad-align-1.1.0.tgz";
      sha1 = "27fa786bcb695fc434ea1500723eb8d0bdc82bf4";
    };
    deps = {
      "get-stdin-4.0.1" = self.by-version."get-stdin"."4.0.1";
      "longest-1.0.1" = self.by-version."longest"."1.0.1";
      "lpad-2.0.1" = self.by-version."lpad"."2.0.1";
      "meow-3.7.0" = self.by-version."meow"."3.7.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lru-cache"."2" =
    self.by-version."lru-cache"."2.7.3";
  by-version."lru-cache"."2.7.3" = self.buildNodePackage {
    name = "lru-cache-2.7.3";
    version = "2.7.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lru-cache/-/lru-cache-2.7.3.tgz";
      name = "lru-cache-2.7.3.tgz";
      sha1 = "6d4524e8b955f95d4f5b58851ce21dd72fb4e952";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lru-cache"."^4.0.1" =
    self.by-version."lru-cache"."4.0.1";
  by-version."lru-cache"."4.0.1" = self.buildNodePackage {
    name = "lru-cache-4.0.1";
    version = "4.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lru-cache/-/lru-cache-4.0.1.tgz";
      name = "lru-cache-4.0.1.tgz";
      sha1 = "1343955edaf2e37d9b9e7ee7241e27c4b9fb72be";
    };
    deps = {
      "pseudomap-1.0.2" = self.by-version."pseudomap"."1.0.2";
      "yallist-2.0.0" = self.by-version."yallist"."2.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."mailcomposer"."~0.2.10" =
    self.by-version."mailcomposer"."0.2.12";
  by-version."mailcomposer"."0.2.12" = self.buildNodePackage {
    name = "mailcomposer-0.2.12";
    version = "0.2.12";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/mailcomposer/-/mailcomposer-0.2.12.tgz";
      name = "mailcomposer-0.2.12.tgz";
      sha1 = "4d02a604616adcb45fb36d37513f4c1bd0b75681";
    };
    deps = {
      "mimelib-0.2.19" = self.by-version."mimelib"."0.2.19";
      "mime-1.2.11" = self.by-version."mime"."1.2.11";
      "he-0.3.6" = self.by-version."he"."0.3.6";
      "follow-redirects-0.0.3" = self.by-version."follow-redirects"."0.0.3";
      "dkim-signer-0.1.2" = self.by-version."dkim-signer"."0.1.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."map-cache"."^0.2.0" =
    self.by-version."map-cache"."0.2.2";
  by-version."map-cache"."0.2.2" = self.buildNodePackage {
    name = "map-cache-0.2.2";
    version = "0.2.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/map-cache/-/map-cache-0.2.2.tgz";
      name = "map-cache-0.2.2.tgz";
      sha1 = "c32abd0bd6525d9b051645bb4f26ac5dc98a0dbf";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."map-obj"."^1.0.0" =
    self.by-version."map-obj"."1.0.1";
  by-version."map-obj"."1.0.1" = self.buildNodePackage {
    name = "map-obj-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/map-obj/-/map-obj-1.0.1.tgz";
      name = "map-obj-1.0.1.tgz";
      sha1 = "d933ceb9205d82bdcf4886f6742bdc2b4dea146d";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."map-obj"."^1.0.1" =
    self.by-version."map-obj"."1.0.1";
  by-spec."map-stream"."~0.1.0" =
    self.by-version."map-stream"."0.1.0";
  by-version."map-stream"."0.1.0" = self.buildNodePackage {
    name = "map-stream-0.1.0";
    version = "0.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/map-stream/-/map-stream-0.1.0.tgz";
      name = "map-stream-0.1.0.tgz";
      sha1 = "e56aa94c4c8055a16404a0674b78f215f7c8e194";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."marked"."^0.3.5" =
    self.by-version."marked"."0.3.6";
  by-version."marked"."0.3.6" = self.buildNodePackage {
    name = "marked-0.3.6";
    version = "0.3.6";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/marked/-/marked-0.3.6.tgz";
      name = "marked-0.3.6.tgz";
      sha1 = "b2c6c618fccece4ef86c4fc6cb8a7cbf5aeda8d7";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."matchdep"."^1.0.0" =
    self.by-version."matchdep"."1.0.1";
  by-version."matchdep"."1.0.1" = self.buildNodePackage {
    name = "matchdep-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/matchdep/-/matchdep-1.0.1.tgz";
      name = "matchdep-1.0.1.tgz";
      sha1 = "a57a33804491fbae208aba8f68380437abc2dca5";
    };
    deps = {
      "findup-sync-0.3.0" = self.by-version."findup-sync"."0.3.0";
      "micromatch-2.3.11" = self.by-version."micromatch"."2.3.11";
      "resolve-1.1.7" = self.by-version."resolve"."1.1.7";
      "stack-trace-0.0.9" = self.by-version."stack-trace"."0.0.9";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."mediaquery-text"."^1.0.0" =
    self.by-version."mediaquery-text"."1.0.2";
  by-version."mediaquery-text"."1.0.2" = self.buildNodePackage {
    name = "mediaquery-text-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/mediaquery-text/-/mediaquery-text-1.0.2.tgz";
      name = "mediaquery-text-1.0.2.tgz";
      sha1 = "3e4598cc3a7db7e96f50ab9c702f1f6cf63180d2";
    };
    deps = {
      "cssom-0.3.1" = self.by-version."cssom"."0.3.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."meow"."3.3.0" =
    self.by-version."meow"."3.3.0";
  by-version."meow"."3.3.0" = self.buildNodePackage {
    name = "meow-3.3.0";
    version = "3.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/meow/-/meow-3.3.0.tgz";
      name = "meow-3.3.0.tgz";
      sha1 = "f8777fd0db67f73d1de1beee08c97c8665efc6ed";
    };
    deps = {
      "camelcase-keys-1.0.0" = self.by-version."camelcase-keys"."1.0.0";
      "indent-string-1.2.2" = self.by-version."indent-string"."1.2.2";
      "minimist-1.2.0" = self.by-version."minimist"."1.2.0";
      "object-assign-3.0.0" = self.by-version."object-assign"."3.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."meow"."^3.1.0" =
    self.by-version."meow"."3.7.0";
  by-version."meow"."3.7.0" = self.buildNodePackage {
    name = "meow-3.7.0";
    version = "3.7.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/meow/-/meow-3.7.0.tgz";
      name = "meow-3.7.0.tgz";
      sha1 = "72cb668b425228290abbfa856892587308a801fb";
    };
    deps = {
      "camelcase-keys-2.1.0" = self.by-version."camelcase-keys"."2.1.0";
      "decamelize-1.2.0" = self.by-version."decamelize"."1.2.0";
      "loud-rejection-1.6.0" = self.by-version."loud-rejection"."1.6.0";
      "map-obj-1.0.1" = self.by-version."map-obj"."1.0.1";
      "minimist-1.2.0" = self.by-version."minimist"."1.2.0";
      "normalize-package-data-2.3.5" = self.by-version."normalize-package-data"."2.3.5";
      "object-assign-4.1.0" = self.by-version."object-assign"."4.1.0";
      "read-pkg-up-1.0.1" = self.by-version."read-pkg-up"."1.0.1";
      "redent-1.0.0" = self.by-version."redent"."1.0.0";
      "trim-newlines-1.0.0" = self.by-version."trim-newlines"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."meow"."^3.3.0" =
    self.by-version."meow"."3.7.0";
  by-spec."meow"."^3.5.0" =
    self.by-version."meow"."3.7.0";
  by-spec."meow"."^3.7.0" =
    self.by-version."meow"."3.7.0";
  by-spec."merge-stream"."^1.0.0" =
    self.by-version."merge-stream"."1.0.0";
  by-version."merge-stream"."1.0.0" = self.buildNodePackage {
    name = "merge-stream-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/merge-stream/-/merge-stream-1.0.0.tgz";
      name = "merge-stream-1.0.0.tgz";
      sha1 = "9cfd156fef35421e2b5403ce11dc6eb1962b026e";
    };
    deps = {
      "readable-stream-2.1.5" = self.by-version."readable-stream"."2.1.5";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."merge-stream"."~1.0.0" =
    self.by-version."merge-stream"."1.0.0";
  "merge-stream" = self.by-version."merge-stream"."1.0.0";
  by-spec."methods"."~1.1.1" =
    self.by-version."methods"."1.1.2";
  by-version."methods"."1.1.2" = self.buildNodePackage {
    name = "methods-1.1.2";
    version = "1.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/methods/-/methods-1.1.2.tgz";
      name = "methods-1.1.2.tgz";
      sha1 = "5529a4d67654134edcc5266656835b0f851afcee";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."micromatch"."2.3.5" =
    self.by-version."micromatch"."2.3.5";
  by-version."micromatch"."2.3.5" = self.buildNodePackage {
    name = "micromatch-2.3.5";
    version = "2.3.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/micromatch/-/micromatch-2.3.5.tgz";
      name = "micromatch-2.3.5.tgz";
      sha1 = "d8dfed89e28419d073489be55c33f0b05c273217";
    };
    deps = {
      "arr-diff-2.0.0" = self.by-version."arr-diff"."2.0.0";
      "array-unique-0.2.1" = self.by-version."array-unique"."0.2.1";
      "braces-1.8.5" = self.by-version."braces"."1.8.5";
      "expand-brackets-0.1.5" = self.by-version."expand-brackets"."0.1.5";
      "extglob-0.3.2" = self.by-version."extglob"."0.3.2";
      "filename-regex-2.0.0" = self.by-version."filename-regex"."2.0.0";
      "is-extglob-1.0.0" = self.by-version."is-extglob"."1.0.0";
      "is-glob-2.0.1" = self.by-version."is-glob"."2.0.1";
      "kind-of-3.0.4" = self.by-version."kind-of"."3.0.4";
      "lazy-cache-0.2.7" = self.by-version."lazy-cache"."0.2.7";
      "normalize-path-2.0.1" = self.by-version."normalize-path"."2.0.1";
      "object.omit-2.0.0" = self.by-version."object.omit"."2.0.0";
      "parse-glob-3.0.4" = self.by-version."parse-glob"."3.0.4";
      "regex-cache-0.4.3" = self.by-version."regex-cache"."0.4.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."micromatch"."^2.1.5" =
    self.by-version."micromatch"."2.3.11";
  by-version."micromatch"."2.3.11" = self.buildNodePackage {
    name = "micromatch-2.3.11";
    version = "2.3.11";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/micromatch/-/micromatch-2.3.11.tgz";
      name = "micromatch-2.3.11.tgz";
      sha1 = "86677c97d1720b363431d04d0d15293bd38c1565";
    };
    deps = {
      "arr-diff-2.0.0" = self.by-version."arr-diff"."2.0.0";
      "array-unique-0.2.1" = self.by-version."array-unique"."0.2.1";
      "braces-1.8.5" = self.by-version."braces"."1.8.5";
      "expand-brackets-0.1.5" = self.by-version."expand-brackets"."0.1.5";
      "extglob-0.3.2" = self.by-version."extglob"."0.3.2";
      "filename-regex-2.0.0" = self.by-version."filename-regex"."2.0.0";
      "is-extglob-1.0.0" = self.by-version."is-extglob"."1.0.0";
      "is-glob-2.0.1" = self.by-version."is-glob"."2.0.1";
      "kind-of-3.0.4" = self.by-version."kind-of"."3.0.4";
      "normalize-path-2.0.1" = self.by-version."normalize-path"."2.0.1";
      "object.omit-2.0.0" = self.by-version."object.omit"."2.0.0";
      "parse-glob-3.0.4" = self.by-version."parse-glob"."3.0.4";
      "regex-cache-0.4.3" = self.by-version."regex-cache"."0.4.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."micromatch"."^2.3.7" =
    self.by-version."micromatch"."2.3.11";
  by-spec."mime"."1.2.4" =
    self.by-version."mime"."1.2.4";
  by-version."mime"."1.2.4" = self.buildNodePackage {
    name = "mime-1.2.4";
    version = "1.2.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/mime/-/mime-1.2.4.tgz";
      name = "mime-1.2.4.tgz";
      sha1 = "11b5fdaf29c2509255176b80ad520294f5de92b7";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."mime"."1.3.4" =
    self.by-version."mime"."1.3.4";
  by-version."mime"."1.3.4" = self.buildNodePackage {
    name = "mime-1.3.4";
    version = "1.3.4";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/mime/-/mime-1.3.4.tgz";
      name = "mime-1.3.4.tgz";
      sha1 = "115f9e3b6b3daf2959983cb38f149a2d40eb5d53";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."mime"."1.x" =
    self.by-version."mime"."1.3.4";
  by-spec."mime".">= 0.0.1" =
    self.by-version."mime"."1.3.4";
  by-spec."mime"."~1.2.11" =
    self.by-version."mime"."1.2.11";
  by-version."mime"."1.2.11" = self.buildNodePackage {
    name = "mime-1.2.11";
    version = "1.2.11";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/mime/-/mime-1.2.11.tgz";
      name = "mime-1.2.11.tgz";
      sha1 = "58203eed86e3a5ef17aed2b7d9ebd47f0a60dd10";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."mime"."~1.2.9" =
    self.by-version."mime"."1.2.11";
  by-spec."mime-db"."~1.12.0" =
    self.by-version."mime-db"."1.12.0";
  by-version."mime-db"."1.12.0" = self.buildNodePackage {
    name = "mime-db-1.12.0";
    version = "1.12.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/mime-db/-/mime-db-1.12.0.tgz";
      name = "mime-db-1.12.0.tgz";
      sha1 = "3d0c63180f458eb10d325aaa37d7c58ae312e9d7";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."mime-db"."~1.23.0" =
    self.by-version."mime-db"."1.23.0";
  by-version."mime-db"."1.23.0" = self.buildNodePackage {
    name = "mime-db-1.23.0";
    version = "1.23.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/mime-db/-/mime-db-1.23.0.tgz";
      name = "mime-db-1.23.0.tgz";
      sha1 = "a31b4070adaea27d732ea333740a64d0ec9a6659";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."mime-types"."^2.1.10" =
    self.by-version."mime-types"."2.1.11";
  by-version."mime-types"."2.1.11" = self.buildNodePackage {
    name = "mime-types-2.1.11";
    version = "2.1.11";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/mime-types/-/mime-types-2.1.11.tgz";
      name = "mime-types-2.1.11.tgz";
      sha1 = "c259c471bda808a85d6cd193b430a5fae4473b3c";
    };
    deps = {
      "mime-db-1.23.0" = self.by-version."mime-db"."1.23.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."mime-types"."^2.1.3" =
    self.by-version."mime-types"."2.1.11";
  by-spec."mime-types"."~2.0.4" =
    self.by-version."mime-types"."2.0.14";
  by-version."mime-types"."2.0.14" = self.buildNodePackage {
    name = "mime-types-2.0.14";
    version = "2.0.14";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/mime-types/-/mime-types-2.0.14.tgz";
      name = "mime-types-2.0.14.tgz";
      sha1 = "310e159db23e077f8bb22b748dabfa4957140aa6";
    };
    deps = {
      "mime-db-1.12.0" = self.by-version."mime-db"."1.12.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."mime-types"."~2.1.11" =
    self.by-version."mime-types"."2.1.11";
  by-spec."mime-types"."~2.1.7" =
    self.by-version."mime-types"."2.1.11";
  by-spec."mimelib"."~0.2.15" =
    self.by-version."mimelib"."0.2.19";
  by-version."mimelib"."0.2.19" = self.buildNodePackage {
    name = "mimelib-0.2.19";
    version = "0.2.19";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/mimelib/-/mimelib-0.2.19.tgz";
      name = "mimelib-0.2.19.tgz";
      sha1 = "37ec90a6ac7d00954851d0b2c31618f0a49da0ee";
    };
    deps = {
      "encoding-0.1.12" = self.by-version."encoding"."0.1.12";
      "addressparser-0.3.2" = self.by-version."addressparser"."0.3.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."minimatch"."0.3" =
    self.by-version."minimatch"."0.3.0";
  by-version."minimatch"."0.3.0" = self.buildNodePackage {
    name = "minimatch-0.3.0";
    version = "0.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/minimatch/-/minimatch-0.3.0.tgz";
      name = "minimatch-0.3.0.tgz";
      sha1 = "275d8edaac4f1bb3326472089e7949c8394699dd";
    };
    deps = {
      "lru-cache-2.7.3" = self.by-version."lru-cache"."2.7.3";
      "sigmund-1.0.1" = self.by-version."sigmund"."1.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."minimatch"."2 || 3" =
    self.by-version."minimatch"."3.0.3";
  by-version."minimatch"."3.0.3" = self.buildNodePackage {
    name = "minimatch-3.0.3";
    version = "3.0.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/minimatch/-/minimatch-3.0.3.tgz";
      name = "minimatch-3.0.3.tgz";
      sha1 = "2a4e4090b96b2db06a9d7df01055a62a77c9b774";
    };
    deps = {
      "brace-expansion-1.1.6" = self.by-version."brace-expansion"."1.1.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."minimatch"."^2.0.1" =
    self.by-version."minimatch"."2.0.10";
  by-version."minimatch"."2.0.10" = self.buildNodePackage {
    name = "minimatch-2.0.10";
    version = "2.0.10";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/minimatch/-/minimatch-2.0.10.tgz";
      name = "minimatch-2.0.10.tgz";
      sha1 = "8d087c39c6b38c001b97fca7ce6d0e1e80afbac7";
    };
    deps = {
      "brace-expansion-1.1.6" = self.by-version."brace-expansion"."1.1.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."minimatch"."^2.0.3" =
    self.by-version."minimatch"."2.0.10";
  by-spec."minimatch"."^3.0.0" =
    self.by-version."minimatch"."3.0.3";
  by-spec."minimatch"."^3.0.2" =
    self.by-version."minimatch"."3.0.3";
  by-spec."minimatch"."~0.2.11" =
    self.by-version."minimatch"."0.2.14";
  by-version."minimatch"."0.2.14" = self.buildNodePackage {
    name = "minimatch-0.2.14";
    version = "0.2.14";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/minimatch/-/minimatch-0.2.14.tgz";
      name = "minimatch-0.2.14.tgz";
      sha1 = "c74e780574f63c6f9a090e90efbe6ef53a6a756a";
    };
    deps = {
      "lru-cache-2.7.3" = self.by-version."lru-cache"."2.7.3";
      "sigmund-1.0.1" = self.by-version."sigmund"."1.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."minimatch"."~3.0.0" =
    self.by-version."minimatch"."3.0.3";
  by-spec."minimist"."0.0.8" =
    self.by-version."minimist"."0.0.8";
  by-version."minimist"."0.0.8" = self.buildNodePackage {
    name = "minimist-0.0.8";
    version = "0.0.8";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/minimist/-/minimist-0.0.8.tgz";
      name = "minimist-0.0.8.tgz";
      sha1 = "857fcabfc3397d2625b8228262e86aa7a011b05d";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."minimist"."^0.2.0" =
    self.by-version."minimist"."0.2.0";
  by-version."minimist"."0.2.0" = self.buildNodePackage {
    name = "minimist-0.2.0";
    version = "0.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/minimist/-/minimist-0.2.0.tgz";
      name = "minimist-0.2.0.tgz";
      sha1 = "4dffe525dae2b864c66c2e23c6271d7afdecefce";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."minimist"."^1.1.0" =
    self.by-version."minimist"."1.2.0";
  by-version."minimist"."1.2.0" = self.buildNodePackage {
    name = "minimist-1.2.0";
    version = "1.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/minimist/-/minimist-1.2.0.tgz";
      name = "minimist-1.2.0.tgz";
      sha1 = "a35008b20f41383eec1fb914f4cd5df79a264284";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."minimist"."^1.1.3" =
    self.by-version."minimist"."1.2.0";
  by-spec."minimist"."^1.2.0" =
    self.by-version."minimist"."1.2.0";
  by-spec."minimist"."~0.0.1" =
    self.by-version."minimist"."0.0.10";
  by-version."minimist"."0.0.10" = self.buildNodePackage {
    name = "minimist-0.0.10";
    version = "0.0.10";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/minimist/-/minimist-0.0.10.tgz";
      name = "minimist-0.0.10.tgz";
      sha1 = "de3f98543dbf96082be48ad1a0c7cda836301dcf";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."mkdirp"."0.3.0" =
    self.by-version."mkdirp"."0.3.0";
  by-version."mkdirp"."0.3.0" = self.buildNodePackage {
    name = "mkdirp-0.3.0";
    version = "0.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/mkdirp/-/mkdirp-0.3.0.tgz";
      name = "mkdirp-0.3.0.tgz";
      sha1 = "1bbf5ab1ba827af23575143490426455f481fe1e";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."mkdirp".">=0.5 0" =
    self.by-version."mkdirp"."0.5.1";
  by-version."mkdirp"."0.5.1" = self.buildNodePackage {
    name = "mkdirp-0.5.1";
    version = "0.5.1";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/mkdirp/-/mkdirp-0.5.1.tgz";
      name = "mkdirp-0.5.1.tgz";
      sha1 = "30057438eac6cf7f8c4767f38648d6697d75c903";
    };
    deps = {
      "minimist-0.0.8" = self.by-version."minimist"."0.0.8";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."mkdirp"."^0.5.0" =
    self.by-version."mkdirp"."0.5.1";
  by-spec."mkdirp"."^0.5.1" =
    self.by-version."mkdirp"."0.5.1";
  by-spec."mkdirp"."~0.5.0" =
    self.by-version."mkdirp"."0.5.1";
  by-spec."mkdirp"."~0.5.1" =
    self.by-version."mkdirp"."0.5.1";
  by-spec."ms"."0.7.1" =
    self.by-version."ms"."0.7.1";
  by-version."ms"."0.7.1" = self.buildNodePackage {
    name = "ms-0.7.1";
    version = "0.7.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/ms/-/ms-0.7.1.tgz";
      name = "ms-0.7.1.tgz";
      sha1 = "9cd13c03adbff25b65effde7ce864ee952017098";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."multiline"."^1.0.2" =
    self.by-version."multiline"."1.0.2";
  by-version."multiline"."1.0.2" = self.buildNodePackage {
    name = "multiline-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/multiline/-/multiline-1.0.2.tgz";
      name = "multiline-1.0.2.tgz";
      sha1 = "69b1f25ff074d2828904f244ddd06b7d96ef6c93";
    };
    deps = {
      "strip-indent-1.0.1" = self.by-version."strip-indent"."1.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."multimatch"."2.0.0" =
    self.by-version."multimatch"."2.0.0";
  by-version."multimatch"."2.0.0" = self.buildNodePackage {
    name = "multimatch-2.0.0";
    version = "2.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/multimatch/-/multimatch-2.0.0.tgz";
      name = "multimatch-2.0.0.tgz";
      sha1 = "c5ada425357b744ba54842ebdce1c8f0be542b6f";
    };
    deps = {
      "array-differ-1.0.0" = self.by-version."array-differ"."1.0.0";
      "array-union-1.0.2" = self.by-version."array-union"."1.0.2";
      "minimatch-2.0.10" = self.by-version."minimatch"."2.0.10";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."multipipe"."^0.1.0" =
    self.by-version."multipipe"."0.1.2";
  by-version."multipipe"."0.1.2" = self.buildNodePackage {
    name = "multipipe-0.1.2";
    version = "0.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/multipipe/-/multipipe-0.1.2.tgz";
      name = "multipipe-0.1.2.tgz";
      sha1 = "2a8f2ddf70eed564dff2d57f1e1a137d9f05078b";
    };
    deps = {
      "duplexer2-0.0.2" = self.by-version."duplexer2"."0.0.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."multipipe"."^0.1.2" =
    self.by-version."multipipe"."0.1.2";
  by-spec."mute-stdout"."^1.0.0" =
    self.by-version."mute-stdout"."1.0.0";
  by-version."mute-stdout"."1.0.0" = self.buildNodePackage {
    name = "mute-stdout-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/mute-stdout/-/mute-stdout-1.0.0.tgz";
      name = "mute-stdout-1.0.0.tgz";
      sha1 = "5b32ea07eb43c9ded6130434cf926f46b2a7fd4d";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."nan"."^2.3.0" =
    self.by-version."nan"."2.4.0";
  by-version."nan"."2.4.0" = self.buildNodePackage {
    name = "nan-2.4.0";
    version = "2.4.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/nan/-/nan-2.4.0.tgz";
      name = "nan-2.4.0.tgz";
      sha1 = "fb3c59d45fe4effe215f0b890f8adf6eb32d2232";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."nan"."^2.3.2" =
    self.by-version."nan"."2.4.0";
  by-spec."negotiator"."0.4.9" =
    self.by-version."negotiator"."0.4.9";
  by-version."negotiator"."0.4.9" = self.buildNodePackage {
    name = "negotiator-0.4.9";
    version = "0.4.9";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/negotiator/-/negotiator-0.4.9.tgz";
      name = "negotiator-0.4.9.tgz";
      sha1 = "92e46b6db53c7e421ed64a2bc94f08be7630df3f";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."negotiator"."0.6.1" =
    self.by-version."negotiator"."0.6.1";
  by-version."negotiator"."0.6.1" = self.buildNodePackage {
    name = "negotiator-0.6.1";
    version = "0.6.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/negotiator/-/negotiator-0.6.1.tgz";
      name = "negotiator-0.6.1.tgz";
      sha1 = "2b327184e8992101177b28563fb5e7102acd0ca9";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."node-gyp"."^3.3.1" =
    self.by-version."node-gyp"."3.4.0";
  by-version."node-gyp"."3.4.0" = self.buildNodePackage {
    name = "node-gyp-3.4.0";
    version = "3.4.0";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/node-gyp/-/node-gyp-3.4.0.tgz";
      name = "node-gyp-3.4.0.tgz";
      sha1 = "dda558393b3ecbbe24c9e6b8703c71194c63fa36";
    };
    deps = {
      "fstream-1.0.10" = self.by-version."fstream"."1.0.10";
      "glob-7.0.5" = self.by-version."glob"."7.0.5";
      "graceful-fs-4.1.5" = self.by-version."graceful-fs"."4.1.5";
      "minimatch-3.0.3" = self.by-version."minimatch"."3.0.3";
      "mkdirp-0.5.1" = self.by-version."mkdirp"."0.5.1";
      "nopt-3.0.6" = self.by-version."nopt"."3.0.6";
      "npmlog-3.1.2" = self.by-version."npmlog"."3.1.2";
      "osenv-0.1.3" = self.by-version."osenv"."0.1.3";
      "path-array-1.0.1" = self.by-version."path-array"."1.0.1";
      "request-2.74.0" = self.by-version."request"."2.74.0";
      "rimraf-2.5.4" = self.by-version."rimraf"."2.5.4";
      "semver-5.3.0" = self.by-version."semver"."5.3.0";
      "tar-2.2.1" = self.by-version."tar"."2.2.1";
      "which-1.2.10" = self.by-version."which"."1.2.10";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."node-pre-gyp"."^0.6.29" =
    self.by-version."node-pre-gyp"."0.6.29";
  by-version."node-pre-gyp"."0.6.29" = self.buildNodePackage {
    name = "node-pre-gyp-0.6.29";
    version = "0.6.29";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/node-pre-gyp/-/node-pre-gyp-0.6.29.tgz";
      name = "node-pre-gyp-0.6.29.tgz";
      sha1 = "b0bd13635baf7d1be7ae233c16fbcf3309acd37c";
    };
    deps = {
      "mkdirp-0.5.1" = self.by-version."mkdirp"."0.5.1";
      "nopt-3.0.6" = self.by-version."nopt"."3.0.6";
      "npmlog-3.1.2" = self.by-version."npmlog"."3.1.2";
      "rc-1.1.6" = self.by-version."rc"."1.1.6";
      "request-2.74.0" = self.by-version."request"."2.74.0";
      "rimraf-2.5.4" = self.by-version."rimraf"."2.5.4";
      "semver-5.2.0" = self.by-version."semver"."5.2.0";
      "tar-2.2.1" = self.by-version."tar"."2.2.1";
      "tar-pack-3.1.4" = self.by-version."tar-pack"."3.1.4";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."node-sass"."^3.4.1" =
    self.by-version."node-sass"."3.8.0";
  by-version."node-sass"."3.8.0" = self.buildNodePackage {
    name = "node-sass-3.8.0";
    version = "3.8.0";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/node-sass/-/node-sass-3.8.0.tgz";
      name = "node-sass-3.8.0.tgz";
      sha1 = "ec0f89ae6625e1d990dc7ff713b275ea15dfee05";
    };
    deps = {
      "async-foreach-0.1.3" = self.by-version."async-foreach"."0.1.3";
      "chalk-1.1.3" = self.by-version."chalk"."1.1.3";
      "cross-spawn-3.0.1" = self.by-version."cross-spawn"."3.0.1";
      "gaze-1.1.1" = self.by-version."gaze"."1.1.1";
      "get-stdin-4.0.1" = self.by-version."get-stdin"."4.0.1";
      "glob-7.0.5" = self.by-version."glob"."7.0.5";
      "in-publish-2.0.0" = self.by-version."in-publish"."2.0.0";
      "lodash.clonedeep-4.5.0" = self.by-version."lodash.clonedeep"."4.5.0";
      "meow-3.7.0" = self.by-version."meow"."3.7.0";
      "mkdirp-0.5.1" = self.by-version."mkdirp"."0.5.1";
      "nan-2.4.0" = self.by-version."nan"."2.4.0";
      "node-gyp-3.4.0" = self.by-version."node-gyp"."3.4.0";
      "request-2.74.0" = self.by-version."request"."2.74.0";
      "sass-graph-2.1.2" = self.by-version."sass-graph"."2.1.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."node-status-codes"."^1.0.0" =
    self.by-version."node-status-codes"."1.0.0";
  by-version."node-status-codes"."1.0.0" = self.buildNodePackage {
    name = "node-status-codes-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/node-status-codes/-/node-status-codes-1.0.0.tgz";
      name = "node-status-codes-1.0.0.tgz";
      sha1 = "5ae5541d024645d32a58fcddc9ceecea7ae3ac2f";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."node-uuid"."~1.4.0" =
    self.by-version."node-uuid"."1.4.7";
  by-version."node-uuid"."1.4.7" = self.buildNodePackage {
    name = "node-uuid-1.4.7";
    version = "1.4.7";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/node-uuid/-/node-uuid-1.4.7.tgz";
      name = "node-uuid-1.4.7.tgz";
      sha1 = "6da5a17668c4b3dd59623bda11cf7fa4c1f60a6f";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."node-uuid"."~1.4.3" =
    self.by-version."node-uuid"."1.4.7";
  by-spec."node-uuid"."~1.4.7" =
    self.by-version."node-uuid"."1.4.7";
  by-spec."nodemailer"."~0.7.0" =
    self.by-version."nodemailer"."0.7.1";
  by-version."nodemailer"."0.7.1" = self.buildNodePackage {
    name = "nodemailer-0.7.1";
    version = "0.7.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/nodemailer/-/nodemailer-0.7.1.tgz";
      name = "nodemailer-0.7.1.tgz";
      sha1 = "1ec819e243622300a00abe746cb5d3389c0f316c";
    };
    deps = {
      "mailcomposer-0.2.12" = self.by-version."mailcomposer"."0.2.12";
      "simplesmtp-0.3.35" = self.by-version."simplesmtp"."0.3.35";
      "directmail-0.1.8" = self.by-version."directmail"."0.1.8";
      "he-0.3.6" = self.by-version."he"."0.3.6";
      "public-address-0.1.1" = self.by-version."public-address"."0.1.1";
      "aws-sdk-2.0.5" = self.by-version."aws-sdk"."2.0.5";
    };
    optionalDependencies = {
      "readable-stream-1.1.14" = self.by-version."readable-stream"."1.1.14";
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."nopt"."2 || 3" =
    self.by-version."nopt"."3.0.6";
  by-version."nopt"."3.0.6" = self.buildNodePackage {
    name = "nopt-3.0.6";
    version = "3.0.6";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/nopt/-/nopt-3.0.6.tgz";
      name = "nopt-3.0.6.tgz";
      sha1 = "c6465dbf08abcd4db359317f79ac68a646b28ff9";
    };
    deps = {
      "abbrev-1.0.9" = self.by-version."abbrev"."1.0.9";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."nopt"."3.0.x" =
    self.by-version."nopt"."3.0.6";
  by-spec."nopt"."~3.0.1" =
    self.by-version."nopt"."3.0.6";
  by-spec."normalize-package-data"."^2.3.2" =
    self.by-version."normalize-package-data"."2.3.5";
  by-version."normalize-package-data"."2.3.5" = self.buildNodePackage {
    name = "normalize-package-data-2.3.5";
    version = "2.3.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/normalize-package-data/-/normalize-package-data-2.3.5.tgz";
      name = "normalize-package-data-2.3.5.tgz";
      sha1 = "8d924f142960e1777e7ffe170543631cc7cb02df";
    };
    deps = {
      "hosted-git-info-2.1.5" = self.by-version."hosted-git-info"."2.1.5";
      "is-builtin-module-1.0.0" = self.by-version."is-builtin-module"."1.0.0";
      "semver-5.3.0" = self.by-version."semver"."5.3.0";
      "validate-npm-package-license-3.0.1" = self.by-version."validate-npm-package-license"."3.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."normalize-package-data"."^2.3.4" =
    self.by-version."normalize-package-data"."2.3.5";
  by-spec."normalize-path"."^2.0.0" =
    self.by-version."normalize-path"."2.0.1";
  by-version."normalize-path"."2.0.1" = self.buildNodePackage {
    name = "normalize-path-2.0.1";
    version = "2.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/normalize-path/-/normalize-path-2.0.1.tgz";
      name = "normalize-path-2.0.1.tgz";
      sha1 = "47886ac1662760d4261b7d979d241709d3ce3f7a";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."normalize-path"."^2.0.1" =
    self.by-version."normalize-path"."2.0.1";
  by-spec."npmlog"."0 || 1 || 2 || 3" =
    self.by-version."npmlog"."3.1.2";
  by-version."npmlog"."3.1.2" = self.buildNodePackage {
    name = "npmlog-3.1.2";
    version = "3.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/npmlog/-/npmlog-3.1.2.tgz";
      name = "npmlog-3.1.2.tgz";
      sha1 = "2d46fa874337af9498a2f12bb43d8d0be4a36873";
    };
    deps = {
      "are-we-there-yet-1.1.2" = self.by-version."are-we-there-yet"."1.1.2";
      "console-control-strings-1.1.0" = self.by-version."console-control-strings"."1.1.0";
      "gauge-2.6.0" = self.by-version."gauge"."2.6.0";
      "set-blocking-2.0.0" = self.by-version."set-blocking"."2.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."npmlog"."~3.1.2" =
    self.by-version."npmlog"."3.1.2";
  by-spec."nth-check"."~1.0.0" =
    self.by-version."nth-check"."1.0.1";
  by-version."nth-check"."1.0.1" = self.buildNodePackage {
    name = "nth-check-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/nth-check/-/nth-check-1.0.1.tgz";
      name = "nth-check-1.0.1.tgz";
      sha1 = "9929acdf628fc2c41098deab82ac580cf149aae4";
    };
    deps = {
      "boolbase-1.0.0" = self.by-version."boolbase"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."nth-check"."~1.0.1" =
    self.by-version."nth-check"."1.0.1";
  by-spec."number-is-nan"."^1.0.0" =
    self.by-version."number-is-nan"."1.0.0";
  by-version."number-is-nan"."1.0.0" = self.buildNodePackage {
    name = "number-is-nan-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/number-is-nan/-/number-is-nan-1.0.0.tgz";
      name = "number-is-nan-1.0.0.tgz";
      sha1 = "c020f529c5282adfdd233d91d4b181c3d686dc4b";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."nwmatcher".">= 1.3.7 < 2.0.0" =
    self.by-version."nwmatcher"."1.3.8";
  by-version."nwmatcher"."1.3.8" = self.buildNodePackage {
    name = "nwmatcher-1.3.8";
    version = "1.3.8";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/nwmatcher/-/nwmatcher-1.3.8.tgz";
      name = "nwmatcher-1.3.8.tgz";
      sha1 = "34edb93de1aa6cb4448b573c9f2a059300241157";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."oauth-sign"."~0.3.0" =
    self.by-version."oauth-sign"."0.3.0";
  by-version."oauth-sign"."0.3.0" = self.buildNodePackage {
    name = "oauth-sign-0.3.0";
    version = "0.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/oauth-sign/-/oauth-sign-0.3.0.tgz";
      name = "oauth-sign-0.3.0.tgz";
      sha1 = "cb540f93bb2b22a7d5941691a288d60e8ea9386e";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."oauth-sign"."~0.8.0" =
    self.by-version."oauth-sign"."0.8.2";
  by-version."oauth-sign"."0.8.2" = self.buildNodePackage {
    name = "oauth-sign-0.8.2";
    version = "0.8.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/oauth-sign/-/oauth-sign-0.8.2.tgz";
      name = "oauth-sign-0.8.2.tgz";
      sha1 = "46a6ab7f0aead8deae9ec0565780b7d4efeb9d43";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."oauth-sign"."~0.8.1" =
    self.by-version."oauth-sign"."0.8.2";
  by-spec."object-assign"."^2.0.0" =
    self.by-version."object-assign"."2.1.1";
  by-version."object-assign"."2.1.1" = self.buildNodePackage {
    name = "object-assign-2.1.1";
    version = "2.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/object-assign/-/object-assign-2.1.1.tgz";
      name = "object-assign-2.1.1.tgz";
      sha1 = "43c36e5d569ff8e4816c4efa8be02d26967c18aa";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."object-assign"."^3.0.0" =
    self.by-version."object-assign"."3.0.0";
  by-version."object-assign"."3.0.0" = self.buildNodePackage {
    name = "object-assign-3.0.0";
    version = "3.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/object-assign/-/object-assign-3.0.0.tgz";
      name = "object-assign-3.0.0.tgz";
      sha1 = "9bedd5ca0897949bca47e7ff408062d549f587f2";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."object-assign"."^4.0.0" =
    self.by-version."object-assign"."4.1.0";
  by-version."object-assign"."4.1.0" = self.buildNodePackage {
    name = "object-assign-4.1.0";
    version = "4.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/object-assign/-/object-assign-4.1.0.tgz";
      name = "object-assign-4.1.0.tgz";
      sha1 = "7a3b3d0e98063d43f4c03f2e8ae6cd51a86883a0";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."object-assign"."^4.0.1" =
    self.by-version."object-assign"."4.1.0";
  by-spec."object-assign"."^4.1.0" =
    self.by-version."object-assign"."4.1.0";
  by-spec."object-component"."0.0.3" =
    self.by-version."object-component"."0.0.3";
  by-version."object-component"."0.0.3" = self.buildNodePackage {
    name = "object-component-0.0.3";
    version = "0.0.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/object-component/-/object-component-0.0.3.tgz";
      name = "object-component-0.0.3.tgz";
      sha1 = "f0c69aa50efc95b866c186f400a33769cb2f1291";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."object-keys"."~0.4.0" =
    self.by-version."object-keys"."0.4.0";
  by-version."object-keys"."0.4.0" = self.buildNodePackage {
    name = "object-keys-0.4.0";
    version = "0.4.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/object-keys/-/object-keys-0.4.0.tgz";
      name = "object-keys-0.4.0.tgz";
      sha1 = "28a6aae7428dd2c3a92f3d95f21335dd204e0336";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."object-path"."^0.9.0" =
    self.by-version."object-path"."0.9.2";
  by-version."object-path"."0.9.2" = self.buildNodePackage {
    name = "object-path-0.9.2";
    version = "0.9.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/object-path/-/object-path-0.9.2.tgz";
      name = "object-path-0.9.2.tgz";
      sha1 = "0fd9a74fc5fad1ae3968b586bda5c632bd6c05a5";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."object-values"."^1.0.0" =
    self.by-version."object-values"."1.0.0";
  by-version."object-values"."1.0.0" = self.buildNodePackage {
    name = "object-values-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/object-values/-/object-values-1.0.0.tgz";
      name = "object-values-1.0.0.tgz";
      sha1 = "72af839630119e5b98c3b02bb8c27e3237158105";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."object.omit"."^2.0.0" =
    self.by-version."object.omit"."2.0.0";
  by-version."object.omit"."2.0.0" = self.buildNodePackage {
    name = "object.omit-2.0.0";
    version = "2.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/object.omit/-/object.omit-2.0.0.tgz";
      name = "object.omit-2.0.0.tgz";
      sha1 = "868597333d54e60662940bb458605dd6ae12fe94";
    };
    deps = {
      "for-own-0.1.4" = self.by-version."for-own"."0.1.4";
      "is-extendable-0.1.1" = self.by-version."is-extendable"."0.1.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."object.pick"."^1.1.1" =
    self.by-version."object.pick"."1.1.2";
  by-version."object.pick"."1.1.2" = self.buildNodePackage {
    name = "object.pick-1.1.2";
    version = "1.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/object.pick/-/object.pick-1.1.2.tgz";
      name = "object.pick-1.1.2.tgz";
      sha1 = "c24da6ce45f10c5aabe71797d16a839cd5969b1e";
    };
    deps = {
      "isobject-2.1.0" = self.by-version."isobject"."2.1.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."on-finished"."~2.3.0" =
    self.by-version."on-finished"."2.3.0";
  by-version."on-finished"."2.3.0" = self.buildNodePackage {
    name = "on-finished-2.3.0";
    version = "2.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/on-finished/-/on-finished-2.3.0.tgz";
      name = "on-finished-2.3.0.tgz";
      sha1 = "20f1336481b083cd75337992a16971aa2d906947";
    };
    deps = {
      "ee-first-1.1.1" = self.by-version."ee-first"."1.1.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."once"."^1.3.0" =
    self.by-version."once"."1.3.3";
  by-version."once"."1.3.3" = self.buildNodePackage {
    name = "once-1.3.3";
    version = "1.3.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/once/-/once-1.3.3.tgz";
      name = "once-1.3.3.tgz";
      sha1 = "b2e261557ce4c314ec8304f3fa82663e4297ca20";
    };
    deps = {
      "wrappy-1.0.2" = self.by-version."wrappy"."1.0.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."once"."^1.3.1" =
    self.by-version."once"."1.3.3";
  by-spec."once"."~1.3.0" =
    self.by-version."once"."1.3.3";
  by-spec."once"."~1.3.3" =
    self.by-version."once"."1.3.3";
  by-spec."onetime"."^1.0.0" =
    self.by-version."onetime"."1.1.0";
  by-version."onetime"."1.1.0" = self.buildNodePackage {
    name = "onetime-1.1.0";
    version = "1.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/onetime/-/onetime-1.1.0.tgz";
      name = "onetime-1.1.0.tgz";
      sha1 = "a1f7838f8314c516f05ecefcbc4ccfe04b4ed789";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."openurl"."1.1.0" =
    self.by-version."openurl"."1.1.0";
  by-version."openurl"."1.1.0" = self.buildNodePackage {
    name = "openurl-1.1.0";
    version = "1.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/openurl/-/openurl-1.1.0.tgz";
      name = "openurl-1.1.0.tgz";
      sha1 = "e2f2189d999c04823201f083f0f1a7cd8903187a";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."opn"."^3.0.2" =
    self.by-version."opn"."3.0.3";
  by-version."opn"."3.0.3" = self.buildNodePackage {
    name = "opn-3.0.3";
    version = "3.0.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/opn/-/opn-3.0.3.tgz";
      name = "opn-3.0.3.tgz";
      sha1 = "b6d99e7399f78d65c3baaffef1fb288e9b85243a";
    };
    deps = {
      "object-assign-4.1.0" = self.by-version."object-assign"."4.1.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."opt-merger"."^1.1.0" =
    self.by-version."opt-merger"."1.1.1";
  by-version."opt-merger"."1.1.1" = self.buildNodePackage {
    name = "opt-merger-1.1.1";
    version = "1.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/opt-merger/-/opt-merger-1.1.1.tgz";
      name = "opt-merger-1.1.1.tgz";
      sha1 = "df4995709941287a8467f9ce58ee06bf4a64ff41";
    };
    deps = {
      "lodash-3.10.1" = self.by-version."lodash"."3.10.1";
      "minimist-1.2.0" = self.by-version."minimist"."1.2.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."optimist"."^0.6.1" =
    self.by-version."optimist"."0.6.1";
  by-version."optimist"."0.6.1" = self.buildNodePackage {
    name = "optimist-0.6.1";
    version = "0.6.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/optimist/-/optimist-0.6.1.tgz";
      name = "optimist-0.6.1.tgz";
      sha1 = "da3ea74686fa21a19a111c326e90eb15a0196686";
    };
    deps = {
      "wordwrap-0.0.3" = self.by-version."wordwrap"."0.0.3";
      "minimist-0.0.10" = self.by-version."minimist"."0.0.10";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."optional"."^0.1.0" =
    self.by-version."optional"."0.1.3";
  by-version."optional"."0.1.3" = self.buildNodePackage {
    name = "optional-0.1.3";
    version = "0.1.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/optional/-/optional-0.1.3.tgz";
      name = "optional-0.1.3.tgz";
      sha1 = "f87537517b59a5e732cfd8f18e4f7eea7ab4761e";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."optionator"."^0.8.1" =
    self.by-version."optionator"."0.8.1";
  by-version."optionator"."0.8.1" = self.buildNodePackage {
    name = "optionator-0.8.1";
    version = "0.8.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/optionator/-/optionator-0.8.1.tgz";
      name = "optionator-0.8.1.tgz";
      sha1 = "e31b4932cdd5fb862a8b0d10bc63d3ee1ec7d78b";
    };
    deps = {
      "prelude-ls-1.1.2" = self.by-version."prelude-ls"."1.1.2";
      "deep-is-0.1.3" = self.by-version."deep-is"."0.1.3";
      "wordwrap-1.0.0" = self.by-version."wordwrap"."1.0.0";
      "type-check-0.3.2" = self.by-version."type-check"."0.3.2";
      "levn-0.3.0" = self.by-version."levn"."0.3.0";
      "fast-levenshtein-1.1.4" = self.by-version."fast-levenshtein"."1.1.4";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."options".">=0.0.5" =
    self.by-version."options"."0.0.6";
  by-version."options"."0.0.6" = self.buildNodePackage {
    name = "options-0.0.6";
    version = "0.0.6";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/options/-/options-0.0.6.tgz";
      name = "options-0.0.6.tgz";
      sha1 = "ec22d312806bb53e731773e7cdaefcf1c643128f";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."optipng-bin"."^3.0.0" =
    self.by-version."optipng-bin"."3.1.2";
  by-version."optipng-bin"."3.1.2" = self.buildNodePackage {
    name = "optipng-bin-3.1.2";
    version = "3.1.2";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/optipng-bin/-/optipng-bin-3.1.2.tgz";
      name = "optipng-bin-3.1.2.tgz";
      sha1 = "18c5a3388ed5d6f1e6ef1998ab0a6bcc8bdd0ca0";
    };
    deps = {
      "bin-build-2.2.0" = self.by-version."bin-build"."2.2.0";
      "bin-wrapper-3.0.2" = self.by-version."bin-wrapper"."3.0.2";
      "logalot-2.1.0" = self.by-version."logalot"."2.1.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."orchestrator"."^0.3.0" =
    self.by-version."orchestrator"."0.3.7";
  by-version."orchestrator"."0.3.7" = self.buildNodePackage {
    name = "orchestrator-0.3.7";
    version = "0.3.7";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/orchestrator/-/orchestrator-0.3.7.tgz";
      name = "orchestrator-0.3.7.tgz";
      sha1 = "c45064e22c5a2a7b99734f409a95ffedc7d3c3df";
    };
    deps = {
      "end-of-stream-0.1.5" = self.by-version."end-of-stream"."0.1.5";
      "sequencify-0.0.7" = self.by-version."sequencify"."0.0.7";
      "stream-consume-0.1.0" = self.by-version."stream-consume"."0.1.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."ordered-read-streams"."^0.1.0" =
    self.by-version."ordered-read-streams"."0.1.0";
  by-version."ordered-read-streams"."0.1.0" = self.buildNodePackage {
    name = "ordered-read-streams-0.1.0";
    version = "0.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/ordered-read-streams/-/ordered-read-streams-0.1.0.tgz";
      name = "ordered-read-streams-0.1.0.tgz";
      sha1 = "fd565a9af8eb4473ba69b6ed8a34352cb552f126";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."ordered-read-streams"."^0.3.0" =
    self.by-version."ordered-read-streams"."0.3.0";
  by-version."ordered-read-streams"."0.3.0" = self.buildNodePackage {
    name = "ordered-read-streams-0.3.0";
    version = "0.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/ordered-read-streams/-/ordered-read-streams-0.3.0.tgz";
      name = "ordered-read-streams-0.3.0.tgz";
      sha1 = "7137e69b3298bb342247a1bbee3881c80e2fd78b";
    };
    deps = {
      "is-stream-1.1.0" = self.by-version."is-stream"."1.1.0";
      "readable-stream-2.1.5" = self.by-version."readable-stream"."2.1.5";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."os-filter-obj"."^1.0.0" =
    self.by-version."os-filter-obj"."1.0.3";
  by-version."os-filter-obj"."1.0.3" = self.buildNodePackage {
    name = "os-filter-obj-1.0.3";
    version = "1.0.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/os-filter-obj/-/os-filter-obj-1.0.3.tgz";
      name = "os-filter-obj-1.0.3.tgz";
      sha1 = "5915330d90eced557d2d938a31c6dd214d9c63ad";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."os-homedir"."^1.0.0" =
    self.by-version."os-homedir"."1.0.1";
  by-version."os-homedir"."1.0.1" = self.buildNodePackage {
    name = "os-homedir-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/os-homedir/-/os-homedir-1.0.1.tgz";
      name = "os-homedir-1.0.1.tgz";
      sha1 = "0d62bdf44b916fd3bbdcf2cab191948fb094f007";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."os-homedir"."^1.0.1" =
    self.by-version."os-homedir"."1.0.1";
  by-spec."os-locale"."^1.4.0" =
    self.by-version."os-locale"."1.4.0";
  by-version."os-locale"."1.4.0" = self.buildNodePackage {
    name = "os-locale-1.4.0";
    version = "1.4.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/os-locale/-/os-locale-1.4.0.tgz";
      name = "os-locale-1.4.0.tgz";
      sha1 = "20f9f17ae29ed345e8bde583b13d2009803c14d9";
    };
    deps = {
      "lcid-1.0.0" = self.by-version."lcid"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."os-tmpdir"."^1.0.0" =
    self.by-version."os-tmpdir"."1.0.1";
  by-version."os-tmpdir"."1.0.1" = self.buildNodePackage {
    name = "os-tmpdir-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/os-tmpdir/-/os-tmpdir-1.0.1.tgz";
      name = "os-tmpdir-1.0.1.tgz";
      sha1 = "e9b423a1edaf479882562e92ed71d7743a071b6e";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."os-tmpdir"."^1.0.1" =
    self.by-version."os-tmpdir"."1.0.1";
  by-spec."osenv"."0" =
    self.by-version."osenv"."0.1.3";
  by-version."osenv"."0.1.3" = self.buildNodePackage {
    name = "osenv-0.1.3";
    version = "0.1.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/osenv/-/osenv-0.1.3.tgz";
      name = "osenv-0.1.3.tgz";
      sha1 = "83cf05c6d6458fc4d5ac6362ea325d92f2754217";
    };
    deps = {
      "os-homedir-1.0.1" = self.by-version."os-homedir"."1.0.1";
      "os-tmpdir-1.0.1" = self.by-version."os-tmpdir"."1.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."osenv"."^0.1.3" =
    self.by-version."osenv"."0.1.3";
  by-spec."pad-component"."0.x" =
    self.by-version."pad-component"."0.0.1";
  by-version."pad-component"."0.0.1" = self.buildNodePackage {
    name = "pad-component-0.0.1";
    version = "0.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/pad-component/-/pad-component-0.0.1.tgz";
      name = "pad-component-0.0.1.tgz";
      sha1 = "ad1f22ce1bf0fdc0d6ddd908af17f351a404b8ac";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."panini"."~1.3.0" =
    self.by-version."panini"."1.3.0";
  by-version."panini"."1.3.0" = self.buildNodePackage {
    name = "panini-1.3.0";
    version = "1.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/panini/-/panini-1.3.0.tgz";
      name = "panini-1.3.0.tgz";
      sha1 = "ec653e38244469533dc17003c6ded1961054e7a0";
    };
    deps = {
      "deepmerge-0.2.10" = self.by-version."deepmerge"."0.2.10";
      "front-matter-2.1.0" = self.by-version."front-matter"."2.1.0";
      "glob-7.0.5" = self.by-version."glob"."7.0.5";
      "handlebars-4.0.5" = self.by-version."handlebars"."4.0.5";
      "highlight.js-8.9.1" = self.by-version."highlight.js"."8.9.1";
      "js-yaml-3.6.1" = self.by-version."js-yaml"."3.6.1";
      "marked-0.3.6" = self.by-version."marked"."0.3.6";
      "through2-2.0.1" = self.by-version."through2"."2.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "panini" = self.by-version."panini"."1.3.0";
  by-spec."param-case"."^1.1.0" =
    self.by-version."param-case"."1.1.2";
  by-version."param-case"."1.1.2" = self.buildNodePackage {
    name = "param-case-1.1.2";
    version = "1.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/param-case/-/param-case-1.1.2.tgz";
      name = "param-case-1.1.2.tgz";
      sha1 = "dcb091a43c259b9228f1c341e7b6a44ea0bf9743";
    };
    deps = {
      "sentence-case-1.1.3" = self.by-version."sentence-case"."1.1.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."parse-filepath"."^1.0.1" =
    self.by-version."parse-filepath"."1.0.1";
  by-version."parse-filepath"."1.0.1" = self.buildNodePackage {
    name = "parse-filepath-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/parse-filepath/-/parse-filepath-1.0.1.tgz";
      name = "parse-filepath-1.0.1.tgz";
      sha1 = "159d6155d43904d16c10ef698911da1e91969b73";
    };
    deps = {
      "is-absolute-0.2.5" = self.by-version."is-absolute"."0.2.5";
      "map-cache-0.2.2" = self.by-version."map-cache"."0.2.2";
      "path-root-0.1.1" = self.by-version."path-root"."0.1.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."parse-glob"."^3.0.4" =
    self.by-version."parse-glob"."3.0.4";
  by-version."parse-glob"."3.0.4" = self.buildNodePackage {
    name = "parse-glob-3.0.4";
    version = "3.0.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/parse-glob/-/parse-glob-3.0.4.tgz";
      name = "parse-glob-3.0.4.tgz";
      sha1 = "b2c376cfb11f35513badd173ef0bb6e3a388391c";
    };
    deps = {
      "glob-base-0.3.0" = self.by-version."glob-base"."0.3.0";
      "is-dotfile-1.0.2" = self.by-version."is-dotfile"."1.0.2";
      "is-extglob-1.0.0" = self.by-version."is-extglob"."1.0.0";
      "is-glob-2.0.1" = self.by-version."is-glob"."2.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."parse-json"."^2.1.0" =
    self.by-version."parse-json"."2.2.0";
  by-version."parse-json"."2.2.0" = self.buildNodePackage {
    name = "parse-json-2.2.0";
    version = "2.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/parse-json/-/parse-json-2.2.0.tgz";
      name = "parse-json-2.2.0.tgz";
      sha1 = "f480f40434ef80741f8469099f8dea18f55a4dc9";
    };
    deps = {
      "error-ex-1.3.0" = self.by-version."error-ex"."1.3.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."parse-json"."^2.2.0" =
    self.by-version."parse-json"."2.2.0";
  by-spec."parse5"."^1.5.1" =
    self.by-version."parse5"."1.5.1";
  by-version."parse5"."1.5.1" = self.buildNodePackage {
    name = "parse5-1.5.1";
    version = "1.5.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/parse5/-/parse5-1.5.1.tgz";
      name = "parse5-1.5.1.tgz";
      sha1 = "9b7f3b0de32be78dc2401b17573ccaf0f6f59d94";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."parsejson"."0.0.1" =
    self.by-version."parsejson"."0.0.1";
  by-version."parsejson"."0.0.1" = self.buildNodePackage {
    name = "parsejson-0.0.1";
    version = "0.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/parsejson/-/parsejson-0.0.1.tgz";
      name = "parsejson-0.0.1.tgz";
      sha1 = "9b10c6c0d825ab589e685153826de0a3ba278bcc";
    };
    deps = {
      "better-assert-1.0.2" = self.by-version."better-assert"."1.0.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."parseqs"."0.0.2" =
    self.by-version."parseqs"."0.0.2";
  by-version."parseqs"."0.0.2" = self.buildNodePackage {
    name = "parseqs-0.0.2";
    version = "0.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/parseqs/-/parseqs-0.0.2.tgz";
      name = "parseqs-0.0.2.tgz";
      sha1 = "9dfe70b2cddac388bde4f35b1f240fa58adbe6c7";
    };
    deps = {
      "better-assert-1.0.2" = self.by-version."better-assert"."1.0.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."parseuri"."0.0.4" =
    self.by-version."parseuri"."0.0.4";
  by-version."parseuri"."0.0.4" = self.buildNodePackage {
    name = "parseuri-0.0.4";
    version = "0.0.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/parseuri/-/parseuri-0.0.4.tgz";
      name = "parseuri-0.0.4.tgz";
      sha1 = "806582a39887e1ea18dd5e2fe0e01902268e9350";
    };
    deps = {
      "better-assert-1.0.2" = self.by-version."better-assert"."1.0.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."parseurl"."~1.3.1" =
    self.by-version."parseurl"."1.3.1";
  by-version."parseurl"."1.3.1" = self.buildNodePackage {
    name = "parseurl-1.3.1";
    version = "1.3.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/parseurl/-/parseurl-1.3.1.tgz";
      name = "parseurl-1.3.1.tgz";
      sha1 = "c8ab8c9223ba34888aa64a297b28853bec18da56";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."pascal-case"."^1.1.0" =
    self.by-version."pascal-case"."1.1.2";
  by-version."pascal-case"."1.1.2" = self.buildNodePackage {
    name = "pascal-case-1.1.2";
    version = "1.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/pascal-case/-/pascal-case-1.1.2.tgz";
      name = "pascal-case-1.1.2.tgz";
      sha1 = "3e5d64a20043830a7c49344c2d74b41be0c9c99b";
    };
    deps = {
      "camel-case-1.2.2" = self.by-version."camel-case"."1.2.2";
      "upper-case-first-1.1.2" = self.by-version."upper-case-first"."1.1.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."path-array"."^1.0.0" =
    self.by-version."path-array"."1.0.1";
  by-version."path-array"."1.0.1" = self.buildNodePackage {
    name = "path-array-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/path-array/-/path-array-1.0.1.tgz";
      name = "path-array-1.0.1.tgz";
      sha1 = "7e2f0f35f07a2015122b868b7eac0eb2c4fec271";
    };
    deps = {
      "array-index-1.0.0" = self.by-version."array-index"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."path-case"."^1.1.0" =
    self.by-version."path-case"."1.1.2";
  by-version."path-case"."1.1.2" = self.buildNodePackage {
    name = "path-case-1.1.2";
    version = "1.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/path-case/-/path-case-1.1.2.tgz";
      name = "path-case-1.1.2.tgz";
      sha1 = "50ce6ba0d3bed3dd0b5c2a9c4553697434409514";
    };
    deps = {
      "sentence-case-1.1.3" = self.by-version."sentence-case"."1.1.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."path-exists"."^1.0.0" =
    self.by-version."path-exists"."1.0.0";
  by-version."path-exists"."1.0.0" = self.buildNodePackage {
    name = "path-exists-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/path-exists/-/path-exists-1.0.0.tgz";
      name = "path-exists-1.0.0.tgz";
      sha1 = "d5a8998eb71ef37a74c34eb0d9eba6e878eea081";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."path-exists"."^2.0.0" =
    self.by-version."path-exists"."2.1.0";
  by-version."path-exists"."2.1.0" = self.buildNodePackage {
    name = "path-exists-2.1.0";
    version = "2.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/path-exists/-/path-exists-2.1.0.tgz";
      name = "path-exists-2.1.0.tgz";
      sha1 = "0feb6c64f0fc518d9a754dd5efb62c7022761f4b";
    };
    deps = {
      "pinkie-promise-2.0.1" = self.by-version."pinkie-promise"."2.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."path-is-absolute"."^1.0.0" =
    self.by-version."path-is-absolute"."1.0.0";
  by-version."path-is-absolute"."1.0.0" = self.buildNodePackage {
    name = "path-is-absolute-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/path-is-absolute/-/path-is-absolute-1.0.0.tgz";
      name = "path-is-absolute-1.0.0.tgz";
      sha1 = "263dada66ab3f2fb10bf7f9d24dd8f3e570ef912";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."path-root"."^0.1.1" =
    self.by-version."path-root"."0.1.1";
  by-version."path-root"."0.1.1" = self.buildNodePackage {
    name = "path-root-0.1.1";
    version = "0.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/path-root/-/path-root-0.1.1.tgz";
      name = "path-root-0.1.1.tgz";
      sha1 = "9a4a6814cac1c0cd73360a95f32083c8ea4745b7";
    };
    deps = {
      "path-root-regex-0.1.2" = self.by-version."path-root-regex"."0.1.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."path-root-regex"."^0.1.0" =
    self.by-version."path-root-regex"."0.1.2";
  by-version."path-root-regex"."0.1.2" = self.buildNodePackage {
    name = "path-root-regex-0.1.2";
    version = "0.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/path-root-regex/-/path-root-regex-0.1.2.tgz";
      name = "path-root-regex-0.1.2.tgz";
      sha1 = "bfccdc8df5b12dc52c8b43ec38d18d72c04ba96d";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."path-type"."^1.0.0" =
    self.by-version."path-type"."1.1.0";
  by-version."path-type"."1.1.0" = self.buildNodePackage {
    name = "path-type-1.1.0";
    version = "1.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/path-type/-/path-type-1.1.0.tgz";
      name = "path-type-1.1.0.tgz";
      sha1 = "59c44f7ee491da704da415da5a4070ba4f8fe441";
    };
    deps = {
      "graceful-fs-4.1.5" = self.by-version."graceful-fs"."4.1.5";
      "pify-2.3.0" = self.by-version."pify"."2.3.0";
      "pinkie-promise-2.0.1" = self.by-version."pinkie-promise"."2.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."pause-stream"."0.0.11" =
    self.by-version."pause-stream"."0.0.11";
  by-version."pause-stream"."0.0.11" = self.buildNodePackage {
    name = "pause-stream-0.0.11";
    version = "0.0.11";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/pause-stream/-/pause-stream-0.0.11.tgz";
      name = "pause-stream-0.0.11.tgz";
      sha1 = "fe5a34b0cbce12b5aa6a2b403ee2e73b602f1445";
    };
    deps = {
      "through-2.3.8" = self.by-version."through"."2.3.8";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."pend"."~1.2.0" =
    self.by-version."pend"."1.2.0";
  by-version."pend"."1.2.0" = self.buildNodePackage {
    name = "pend-1.2.0";
    version = "1.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/pend/-/pend-1.2.0.tgz";
      name = "pend-1.2.0.tgz";
      sha1 = "7a57eb550a6783f9115331fcf4663d5c8e007a50";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."pify"."^2.0.0" =
    self.by-version."pify"."2.3.0";
  by-version."pify"."2.3.0" = self.buildNodePackage {
    name = "pify-2.3.0";
    version = "2.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/pify/-/pify-2.3.0.tgz";
      name = "pify-2.3.0.tgz";
      sha1 = "ed141a6ac043a849ea588498e7dca8b15330e90c";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."pinkie"."^2.0.0" =
    self.by-version."pinkie"."2.0.4";
  by-version."pinkie"."2.0.4" = self.buildNodePackage {
    name = "pinkie-2.0.4";
    version = "2.0.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/pinkie/-/pinkie-2.0.4.tgz";
      name = "pinkie-2.0.4.tgz";
      sha1 = "72556b80cfa0d48a974e80e77248e80ed4f7f870";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."pinkie-promise"."^2.0.0" =
    self.by-version."pinkie-promise"."2.0.1";
  by-version."pinkie-promise"."2.0.1" = self.buildNodePackage {
    name = "pinkie-promise-2.0.1";
    version = "2.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/pinkie-promise/-/pinkie-promise-2.0.1.tgz";
      name = "pinkie-promise-2.0.1.tgz";
      sha1 = "2135d6dfa7a358c069ac9b178776288228450ffa";
    };
    deps = {
      "pinkie-2.0.4" = self.by-version."pinkie"."2.0.4";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."pkg-conf"."^1.1.1" =
    self.by-version."pkg-conf"."1.1.3";
  by-version."pkg-conf"."1.1.3" = self.buildNodePackage {
    name = "pkg-conf-1.1.3";
    version = "1.1.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/pkg-conf/-/pkg-conf-1.1.3.tgz";
      name = "pkg-conf-1.1.3.tgz";
      sha1 = "378e56d6fd13e88bfb6f4a25df7a83faabddba5b";
    };
    deps = {
      "find-up-1.1.2" = self.by-version."find-up"."1.1.2";
      "load-json-file-1.1.0" = self.by-version."load-json-file"."1.1.0";
      "object-assign-4.1.0" = self.by-version."object-assign"."4.1.0";
      "symbol-0.2.3" = self.by-version."symbol"."0.2.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."plur"."^2.0.0" =
    self.by-version."plur"."2.1.2";
  by-version."plur"."2.1.2" = self.buildNodePackage {
    name = "plur-2.1.2";
    version = "2.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/plur/-/plur-2.1.2.tgz";
      name = "plur-2.1.2.tgz";
      sha1 = "7482452c1a0f508e3e344eaec312c91c29dc655a";
    };
    deps = {
      "irregular-plurals-1.2.0" = self.by-version."irregular-plurals"."1.2.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."portscanner"."^1.0.0" =
    self.by-version."portscanner"."1.0.0";
  by-version."portscanner"."1.0.0" = self.buildNodePackage {
    name = "portscanner-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/portscanner/-/portscanner-1.0.0.tgz";
      name = "portscanner-1.0.0.tgz";
      sha1 = "3b5cfe393828b5160abc600e6270ebc2f1590558";
    };
    deps = {
      "async-0.1.15" = self.by-version."async"."0.1.15";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."prelude-ls"."~1.1.2" =
    self.by-version."prelude-ls"."1.1.2";
  by-version."prelude-ls"."1.1.2" = self.buildNodePackage {
    name = "prelude-ls-1.1.2";
    version = "1.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/prelude-ls/-/prelude-ls-1.1.2.tgz";
      name = "prelude-ls-1.1.2.tgz";
      sha1 = "21932a549f5e52ffd9a827f570e04be62a97da54";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."prepend-http"."^1.0.1" =
    self.by-version."prepend-http"."1.0.4";
  by-version."prepend-http"."1.0.4" = self.buildNodePackage {
    name = "prepend-http-1.0.4";
    version = "1.0.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/prepend-http/-/prepend-http-1.0.4.tgz";
      name = "prepend-http-1.0.4.tgz";
      sha1 = "d4f4562b0ce3696e41ac52d0e002e57a635dc6dc";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."preserve"."^0.2.0" =
    self.by-version."preserve"."0.2.0";
  by-version."preserve"."0.2.0" = self.buildNodePackage {
    name = "preserve-0.2.0";
    version = "0.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/preserve/-/preserve-0.2.0.tgz";
      name = "preserve-0.2.0.tgz";
      sha1 = "815ed1f6ebc65926f865b310c0713bcb3315ce4b";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."pretty-bytes"."^2.0.1" =
    self.by-version."pretty-bytes"."2.0.1";
  by-version."pretty-bytes"."2.0.1" = self.buildNodePackage {
    name = "pretty-bytes-2.0.1";
    version = "2.0.1";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/pretty-bytes/-/pretty-bytes-2.0.1.tgz";
      name = "pretty-bytes-2.0.1.tgz";
      sha1 = "155ec4d0036f41391e7045d6dbe4963d525d264f";
    };
    deps = {
      "get-stdin-4.0.1" = self.by-version."get-stdin"."4.0.1";
      "meow-3.7.0" = self.by-version."meow"."3.7.0";
      "number-is-nan-1.0.0" = self.by-version."number-is-nan"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."pretty-hrtime"."^1.0.0" =
    self.by-version."pretty-hrtime"."1.0.2";
  by-version."pretty-hrtime"."1.0.2" = self.buildNodePackage {
    name = "pretty-hrtime-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/pretty-hrtime/-/pretty-hrtime-1.0.2.tgz";
      name = "pretty-hrtime-1.0.2.tgz";
      sha1 = "70ca96f4d0628a443b918758f79416a9a7bc9fa8";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."private"."^0.1.6" =
    self.by-version."private"."0.1.6";
  by-version."private"."0.1.6" = self.buildNodePackage {
    name = "private-0.1.6";
    version = "0.1.6";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/private/-/private-0.1.6.tgz";
      name = "private-0.1.6.tgz";
      sha1 = "55c6a976d0f9bafb9924851350fe47b9b5fbb7c1";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."private"."~0.1.5" =
    self.by-version."private"."0.1.6";
  by-spec."process-nextick-args"."~1.0.6" =
    self.by-version."process-nextick-args"."1.0.7";
  by-version."process-nextick-args"."1.0.7" = self.buildNodePackage {
    name = "process-nextick-args-1.0.7";
    version = "1.0.7";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/process-nextick-args/-/process-nextick-args-1.0.7.tgz";
      name = "process-nextick-args-1.0.7.tgz";
      sha1 = "150e20b756590ad3f91093f25a4f2ad8bff30ba3";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."pseudomap"."^1.0.1" =
    self.by-version."pseudomap"."1.0.2";
  by-version."pseudomap"."1.0.2" = self.buildNodePackage {
    name = "pseudomap-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/pseudomap/-/pseudomap-1.0.2.tgz";
      name = "pseudomap-1.0.2.tgz";
      sha1 = "f052a28da70e618917ef0a8ac34c1ae5a68286b3";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."public-address"."~0.1.1" =
    self.by-version."public-address"."0.1.1";
  by-version."public-address"."0.1.1" = self.buildNodePackage {
    name = "public-address-0.1.1";
    version = "0.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/public-address/-/public-address-0.1.1.tgz";
      name = "public-address-0.1.1.tgz";
      sha1 = "58bdea323c88287b6914159312454b804ca9eeaf";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."pump"."^1.0.0" =
    self.by-version."pump"."1.0.1";
  by-version."pump"."1.0.1" = self.buildNodePackage {
    name = "pump-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/pump/-/pump-1.0.1.tgz";
      name = "pump-1.0.1.tgz";
      sha1 = "f1f1409fb9bd1085bbdb576b43b84ec4b5eadc1a";
    };
    deps = {
      "end-of-stream-1.1.0" = self.by-version."end-of-stream"."1.1.0";
      "once-1.3.3" = self.by-version."once"."1.3.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."pumpify"."^1.2.1" =
    self.by-version."pumpify"."1.3.5";
  by-version."pumpify"."1.3.5" = self.buildNodePackage {
    name = "pumpify-1.3.5";
    version = "1.3.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/pumpify/-/pumpify-1.3.5.tgz";
      name = "pumpify-1.3.5.tgz";
      sha1 = "1b671c619940abcaeac0ad0e3a3c164be760993b";
    };
    deps = {
      "duplexify-3.4.5" = self.by-version."duplexify"."3.4.5";
      "inherits-2.0.1" = self.by-version."inherits"."2.0.1";
      "pump-1.0.1" = self.by-version."pump"."1.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."punycode"."~1.2.4" =
    self.by-version."punycode"."1.2.4";
  by-version."punycode"."1.2.4" = self.buildNodePackage {
    name = "punycode-1.2.4";
    version = "1.2.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/punycode/-/punycode-1.2.4.tgz";
      name = "punycode-1.2.4.tgz";
      sha1 = "54008ac972aec74175def9cba6df7fa9d3918740";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."q"."^1.1.2" =
    self.by-version."q"."1.4.1";
  by-version."q"."1.4.1" = self.buildNodePackage {
    name = "q-1.4.1";
    version = "1.4.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/q/-/q-1.4.1.tgz";
      name = "q-1.4.1.tgz";
      sha1 = "55705bcd93c5f3673530c2c2cbc0c2b3addc286e";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."q"."~1.0.1" =
    self.by-version."q"."1.0.1";
  by-version."q"."1.0.1" = self.buildNodePackage {
    name = "q-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/q/-/q-1.0.1.tgz";
      name = "q-1.0.1.tgz";
      sha1 = "11872aeedee89268110b10a718448ffb10112a14";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."qs"."0.4.x" =
    self.by-version."qs"."0.4.2";
  by-version."qs"."0.4.2" = self.buildNodePackage {
    name = "qs-0.4.2";
    version = "0.4.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/qs/-/qs-0.4.2.tgz";
      name = "qs-0.4.2.tgz";
      sha1 = "3cac4c861e371a8c9c4770ac23cda8de639b8e5f";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."qs"."2.3.3" =
    self.by-version."qs"."2.3.3";
  by-version."qs"."2.3.3" = self.buildNodePackage {
    name = "qs-2.3.3";
    version = "2.3.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/qs/-/qs-2.3.3.tgz";
      name = "qs-2.3.3.tgz";
      sha1 = "e9e85adbe75da0bbe4c8e0476a086290f863b404";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."qs".">= 0.4.0" =
    self.by-version."qs"."6.2.1";
  by-version."qs"."6.2.1" = self.buildNodePackage {
    name = "qs-6.2.1";
    version = "6.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/qs/-/qs-6.2.1.tgz";
      name = "qs-6.2.1.tgz";
      sha1 = "ce03c5ff0935bc1d9d69a9f14cbd18e568d67625";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."qs"."~0.6.0" =
    self.by-version."qs"."0.6.6";
  by-version."qs"."0.6.6" = self.buildNodePackage {
    name = "qs-0.6.6";
    version = "0.6.6";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/qs/-/qs-0.6.6.tgz";
      name = "qs-0.6.6.tgz";
      sha1 = "6e015098ff51968b8a3c819001d5f2c89bc4b107";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."qs"."~5.2.0" =
    self.by-version."qs"."5.2.1";
  by-version."qs"."5.2.1" = self.buildNodePackage {
    name = "qs-5.2.1";
    version = "5.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/qs/-/qs-5.2.1.tgz";
      name = "qs-5.2.1.tgz";
      sha1 = "801fee030e0b9450d6385adc48a4cc55b44aedfc";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."qs"."~6.2.0" =
    self.by-version."qs"."6.2.1";
  by-spec."query-string"."^2.4.0" =
    self.by-version."query-string"."2.4.2";
  by-version."query-string"."2.4.2" = self.buildNodePackage {
    name = "query-string-2.4.2";
    version = "2.4.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/query-string/-/query-string-2.4.2.tgz";
      name = "query-string-2.4.2.tgz";
      sha1 = "7db0666420804baa92ae9f268962855a76143dfb";
    };
    deps = {
      "strict-uri-encode-1.1.0" = self.by-version."strict-uri-encode"."1.1.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."rai"."~0.1.11" =
    self.by-version."rai"."0.1.12";
  by-version."rai"."0.1.12" = self.buildNodePackage {
    name = "rai-0.1.12";
    version = "0.1.12";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/rai/-/rai-0.1.12.tgz";
      name = "rai-0.1.12.tgz";
      sha1 = "8ccfd014d0f9608630dd73c19b8e4b057754a6a6";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."randomatic"."^1.1.3" =
    self.by-version."randomatic"."1.1.5";
  by-version."randomatic"."1.1.5" = self.buildNodePackage {
    name = "randomatic-1.1.5";
    version = "1.1.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/randomatic/-/randomatic-1.1.5.tgz";
      name = "randomatic-1.1.5.tgz";
      sha1 = "5e9ef5f2d573c67bd2b8124ae90b5156e457840b";
    };
    deps = {
      "is-number-2.1.0" = self.by-version."is-number"."2.1.0";
      "kind-of-3.0.4" = self.by-version."kind-of"."3.0.4";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."range-parser"."~1.2.0" =
    self.by-version."range-parser"."1.2.0";
  by-version."range-parser"."1.2.0" = self.buildNodePackage {
    name = "range-parser-1.2.0";
    version = "1.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/range-parser/-/range-parser-1.2.0.tgz";
      name = "range-parser-1.2.0.tgz";
      sha1 = "f49be6b487894ddc40dcc94a322f611092e00d5e";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."rc"."^1.1.2" =
    self.by-version."rc"."1.1.6";
  by-version."rc"."1.1.6" = self.buildNodePackage {
    name = "rc-1.1.6";
    version = "1.1.6";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/rc/-/rc-1.1.6.tgz";
      name = "rc-1.1.6.tgz";
      sha1 = "43651b76b6ae53b5c802f1151fa3fc3b059969c9";
    };
    deps = {
      "deep-extend-0.4.1" = self.by-version."deep-extend"."0.4.1";
      "ini-1.3.4" = self.by-version."ini"."1.3.4";
      "minimist-1.2.0" = self.by-version."minimist"."1.2.0";
      "strip-json-comments-1.0.4" = self.by-version."strip-json-comments"."1.0.4";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."rc"."~1.1.0" =
    self.by-version."rc"."1.1.6";
  by-spec."read-all-stream"."^3.0.0" =
    self.by-version."read-all-stream"."3.1.0";
  by-version."read-all-stream"."3.1.0" = self.buildNodePackage {
    name = "read-all-stream-3.1.0";
    version = "3.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/read-all-stream/-/read-all-stream-3.1.0.tgz";
      name = "read-all-stream-3.1.0.tgz";
      sha1 = "35c3e177f2078ef789ee4bfafa4373074eaef4fa";
    };
    deps = {
      "pinkie-promise-2.0.1" = self.by-version."pinkie-promise"."2.0.1";
      "readable-stream-2.1.5" = self.by-version."readable-stream"."2.1.5";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."read-pkg"."^1.0.0" =
    self.by-version."read-pkg"."1.1.0";
  by-version."read-pkg"."1.1.0" = self.buildNodePackage {
    name = "read-pkg-1.1.0";
    version = "1.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/read-pkg/-/read-pkg-1.1.0.tgz";
      name = "read-pkg-1.1.0.tgz";
      sha1 = "f5ffaa5ecd29cb31c0474bca7d756b6bb29e3f28";
    };
    deps = {
      "load-json-file-1.1.0" = self.by-version."load-json-file"."1.1.0";
      "normalize-package-data-2.3.5" = self.by-version."normalize-package-data"."2.3.5";
      "path-type-1.1.0" = self.by-version."path-type"."1.1.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."read-pkg-up"."^1.0.1" =
    self.by-version."read-pkg-up"."1.0.1";
  by-version."read-pkg-up"."1.0.1" = self.buildNodePackage {
    name = "read-pkg-up-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/read-pkg-up/-/read-pkg-up-1.0.1.tgz";
      name = "read-pkg-up-1.0.1.tgz";
      sha1 = "9d63c13276c065918d57f002a57f40a1b643fb02";
    };
    deps = {
      "find-up-1.1.2" = self.by-version."find-up"."1.1.2";
      "read-pkg-1.1.0" = self.by-version."read-pkg"."1.1.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."readable-stream"."1.0.27-1" =
    self.by-version."readable-stream"."1.0.27-1";
  by-version."readable-stream"."1.0.27-1" = self.buildNodePackage {
    name = "readable-stream-1.0.27-1";
    version = "1.0.27-1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/readable-stream/-/readable-stream-1.0.27-1.tgz";
      name = "readable-stream-1.0.27-1.tgz";
      sha1 = "6b67983c20357cefd07f0165001a16d710d91078";
    };
    deps = {
      "core-util-is-1.0.2" = self.by-version."core-util-is"."1.0.2";
      "isarray-0.0.1" = self.by-version."isarray"."0.0.1";
      "string_decoder-0.10.31" = self.by-version."string_decoder"."0.10.31";
      "inherits-2.0.1" = self.by-version."inherits"."2.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."readable-stream"."1.1" =
    self.by-version."readable-stream"."1.1.14";
  by-version."readable-stream"."1.1.14" = self.buildNodePackage {
    name = "readable-stream-1.1.14";
    version = "1.1.14";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/readable-stream/-/readable-stream-1.1.14.tgz";
      name = "readable-stream-1.1.14.tgz";
      sha1 = "7cf4c54ef648e3813084c636dd2079e166c081d9";
    };
    deps = {
      "core-util-is-1.0.2" = self.by-version."core-util-is"."1.0.2";
      "isarray-0.0.1" = self.by-version."isarray"."0.0.1";
      "string_decoder-0.10.31" = self.by-version."string_decoder"."0.10.31";
      "inherits-2.0.1" = self.by-version."inherits"."2.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."readable-stream".">=1.0.33-1 <1.1.0-0" =
    self.by-version."readable-stream"."1.0.34";
  by-version."readable-stream"."1.0.34" = self.buildNodePackage {
    name = "readable-stream-1.0.34";
    version = "1.0.34";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/readable-stream/-/readable-stream-1.0.34.tgz";
      name = "readable-stream-1.0.34.tgz";
      sha1 = "125820e34bc842d2f2aaafafe4c2916ee32c157c";
    };
    deps = {
      "core-util-is-1.0.2" = self.by-version."core-util-is"."1.0.2";
      "isarray-0.0.1" = self.by-version."isarray"."0.0.1";
      "string_decoder-0.10.31" = self.by-version."string_decoder"."0.10.31";
      "inherits-2.0.1" = self.by-version."inherits"."2.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."readable-stream"."^2.0.0" =
    self.by-version."readable-stream"."2.1.5";
  by-version."readable-stream"."2.1.5" = self.buildNodePackage {
    name = "readable-stream-2.1.5";
    version = "2.1.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/readable-stream/-/readable-stream-2.1.5.tgz";
      name = "readable-stream-2.1.5.tgz";
      sha1 = "66fa8b720e1438b364681f2ad1a63c618448c9d0";
    };
    deps = {
      "buffer-shims-1.0.0" = self.by-version."buffer-shims"."1.0.0";
      "core-util-is-1.0.2" = self.by-version."core-util-is"."1.0.2";
      "inherits-2.0.1" = self.by-version."inherits"."2.0.1";
      "isarray-1.0.0" = self.by-version."isarray"."1.0.0";
      "process-nextick-args-1.0.7" = self.by-version."process-nextick-args"."1.0.7";
      "string_decoder-0.10.31" = self.by-version."string_decoder"."0.10.31";
      "util-deprecate-1.0.2" = self.by-version."util-deprecate"."1.0.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."readable-stream"."^2.0.0 || ^1.1.13" =
    self.by-version."readable-stream"."2.1.5";
  by-spec."readable-stream"."^2.0.1" =
    self.by-version."readable-stream"."2.1.5";
  by-spec."readable-stream"."^2.0.2" =
    self.by-version."readable-stream"."2.1.5";
  by-spec."readable-stream"."^2.0.4" =
    self.by-version."readable-stream"."2.1.5";
  by-spec."readable-stream"."^2.0.5" =
    self.by-version."readable-stream"."2.1.5";
  by-spec."readable-stream"."~1.0.17" =
    self.by-version."readable-stream"."1.0.34";
  by-spec."readable-stream"."~1.1.9" =
    self.by-version."readable-stream"."1.1.14";
  by-spec."readable-stream"."~2.0.0" =
    self.by-version."readable-stream"."2.0.6";
  by-version."readable-stream"."2.0.6" = self.buildNodePackage {
    name = "readable-stream-2.0.6";
    version = "2.0.6";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/readable-stream/-/readable-stream-2.0.6.tgz";
      name = "readable-stream-2.0.6.tgz";
      sha1 = "8f90341e68a53ccc928788dacfcd11b36eb9b78e";
    };
    deps = {
      "core-util-is-1.0.2" = self.by-version."core-util-is"."1.0.2";
      "inherits-2.0.1" = self.by-version."inherits"."2.0.1";
      "isarray-1.0.0" = self.by-version."isarray"."1.0.0";
      "process-nextick-args-1.0.7" = self.by-version."process-nextick-args"."1.0.7";
      "string_decoder-0.10.31" = self.by-version."string_decoder"."0.10.31";
      "util-deprecate-1.0.2" = self.by-version."util-deprecate"."1.0.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."readable-stream"."~2.0.5" =
    self.by-version."readable-stream"."2.0.6";
  by-spec."readable-stream"."~2.1.4" =
    self.by-version."readable-stream"."2.1.5";
  by-spec."readdirp"."^2.0.0" =
    self.by-version."readdirp"."2.1.0";
  by-version."readdirp"."2.1.0" = self.buildNodePackage {
    name = "readdirp-2.1.0";
    version = "2.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/readdirp/-/readdirp-2.1.0.tgz";
      name = "readdirp-2.1.0.tgz";
      sha1 = "4ed0ad060df3073300c48440373f72d1cc642d78";
    };
    deps = {
      "graceful-fs-4.1.5" = self.by-version."graceful-fs"."4.1.5";
      "minimatch-3.0.3" = self.by-version."minimatch"."3.0.3";
      "readable-stream-2.1.5" = self.by-version."readable-stream"."2.1.5";
      "set-immediate-shim-1.0.1" = self.by-version."set-immediate-shim"."1.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."rechoir"."^0.6.2" =
    self.by-version."rechoir"."0.6.2";
  by-version."rechoir"."0.6.2" = self.buildNodePackage {
    name = "rechoir-0.6.2";
    version = "0.6.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/rechoir/-/rechoir-0.6.2.tgz";
      name = "rechoir-0.6.2.tgz";
      sha1 = "85204b54dba82d5742e28c96756ef43af50e3384";
    };
    deps = {
      "resolve-1.1.7" = self.by-version."resolve"."1.1.7";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."redent"."^1.0.0" =
    self.by-version."redent"."1.0.0";
  by-version."redent"."1.0.0" = self.buildNodePackage {
    name = "redent-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/redent/-/redent-1.0.0.tgz";
      name = "redent-1.0.0.tgz";
      sha1 = "cf916ab1fd5f1f16dfb20822dd6ec7f730c2afde";
    };
    deps = {
      "indent-string-2.1.0" = self.by-version."indent-string"."2.1.0";
      "strip-indent-1.0.1" = self.by-version."strip-indent"."1.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."reduce-component"."1.0.1" =
    self.by-version."reduce-component"."1.0.1";
  by-version."reduce-component"."1.0.1" = self.buildNodePackage {
    name = "reduce-component-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/reduce-component/-/reduce-component-1.0.1.tgz";
      name = "reduce-component-1.0.1.tgz";
      sha1 = "e0c93542c574521bea13df0f9488ed82ab77c5da";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."regenerate"."^1.2.1" =
    self.by-version."regenerate"."1.3.1";
  by-version."regenerate"."1.3.1" = self.buildNodePackage {
    name = "regenerate-1.3.1";
    version = "1.3.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/regenerate/-/regenerate-1.3.1.tgz";
      name = "regenerate-1.3.1.tgz";
      sha1 = "0300203a5d2fdcf89116dce84275d011f5903f33";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."regenerator-runtime"."^0.9.5" =
    self.by-version."regenerator-runtime"."0.9.5";
  by-version."regenerator-runtime"."0.9.5" = self.buildNodePackage {
    name = "regenerator-runtime-0.9.5";
    version = "0.9.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/regenerator-runtime/-/regenerator-runtime-0.9.5.tgz";
      name = "regenerator-runtime-0.9.5.tgz";
      sha1 = "403d6d40a4bdff9c330dd9392dcbb2d9a8bba1fc";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."regex-cache"."^0.4.2" =
    self.by-version."regex-cache"."0.4.3";
  by-version."regex-cache"."0.4.3" = self.buildNodePackage {
    name = "regex-cache-0.4.3";
    version = "0.4.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/regex-cache/-/regex-cache-0.4.3.tgz";
      name = "regex-cache-0.4.3.tgz";
      sha1 = "9b1a6c35d4d0dfcef5711ae651e8e9d3d7114145";
    };
    deps = {
      "is-equal-shallow-0.1.3" = self.by-version."is-equal-shallow"."0.1.3";
      "is-primitive-2.0.0" = self.by-version."is-primitive"."2.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."regexpu-core"."^2.0.0" =
    self.by-version."regexpu-core"."2.0.0";
  by-version."regexpu-core"."2.0.0" = self.buildNodePackage {
    name = "regexpu-core-2.0.0";
    version = "2.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/regexpu-core/-/regexpu-core-2.0.0.tgz";
      name = "regexpu-core-2.0.0.tgz";
      sha1 = "49d038837b8dcf8bfa5b9a42139938e6ea2ae240";
    };
    deps = {
      "regenerate-1.3.1" = self.by-version."regenerate"."1.3.1";
      "regjsgen-0.2.0" = self.by-version."regjsgen"."0.2.0";
      "regjsparser-0.1.5" = self.by-version."regjsparser"."0.1.5";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."regjsgen"."^0.2.0" =
    self.by-version."regjsgen"."0.2.0";
  by-version."regjsgen"."0.2.0" = self.buildNodePackage {
    name = "regjsgen-0.2.0";
    version = "0.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/regjsgen/-/regjsgen-0.2.0.tgz";
      name = "regjsgen-0.2.0.tgz";
      sha1 = "6c016adeac554f75823fe37ac05b92d5a4edb1f7";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."regjsparser"."^0.1.4" =
    self.by-version."regjsparser"."0.1.5";
  by-version."regjsparser"."0.1.5" = self.buildNodePackage {
    name = "regjsparser-0.1.5";
    version = "0.1.5";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/regjsparser/-/regjsparser-0.1.5.tgz";
      name = "regjsparser-0.1.5.tgz";
      sha1 = "7ee8f84dc6fa792d3fd0ae228d24bd949ead205c";
    };
    deps = {
      "jsesc-0.5.0" = self.by-version."jsesc"."0.5.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."relateurl"."0.2.x" =
    self.by-version."relateurl"."0.2.7";
  by-version."relateurl"."0.2.7" = self.buildNodePackage {
    name = "relateurl-0.2.7";
    version = "0.2.7";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/relateurl/-/relateurl-0.2.7.tgz";
      name = "relateurl-0.2.7.tgz";
      sha1 = "54dbf377e51440aca90a4cd274600d3ff2d888a9";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."remote-content"."^1.0.0" =
    self.by-version."remote-content"."1.0.1";
  by-version."remote-content"."1.0.1" = self.buildNodePackage {
    name = "remote-content-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/remote-content/-/remote-content-1.0.1.tgz";
      name = "remote-content-1.0.1.tgz";
      sha1 = "0c370e36701edc2281d7d1722d4273083d137163";
    };
    deps = {
      "superagent-1.8.4" = self.by-version."superagent"."1.8.4";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."repeat-element"."^1.1.2" =
    self.by-version."repeat-element"."1.1.2";
  by-version."repeat-element"."1.1.2" = self.buildNodePackage {
    name = "repeat-element-1.1.2";
    version = "1.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/repeat-element/-/repeat-element-1.1.2.tgz";
      name = "repeat-element-1.1.2.tgz";
      sha1 = "ef089a178d1483baae4d93eb98b4f9e4e11d990a";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."repeat-string"."^1.5.2" =
    self.by-version."repeat-string"."1.5.4";
  by-version."repeat-string"."1.5.4" = self.buildNodePackage {
    name = "repeat-string-1.5.4";
    version = "1.5.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/repeat-string/-/repeat-string-1.5.4.tgz";
      name = "repeat-string-1.5.4.tgz";
      sha1 = "64ec0c91e0f4b475f90d5b643651e3e6e5b6c2d5";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."repeating"."^1.1.0" =
    self.by-version."repeating"."1.1.3";
  by-version."repeating"."1.1.3" = self.buildNodePackage {
    name = "repeating-1.1.3";
    version = "1.1.3";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/repeating/-/repeating-1.1.3.tgz";
      name = "repeating-1.1.3.tgz";
      sha1 = "3d4114218877537494f97f77f9785fab810fa4ac";
    };
    deps = {
      "is-finite-1.0.1" = self.by-version."is-finite"."1.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."repeating"."^2.0.0" =
    self.by-version."repeating"."2.0.1";
  by-version."repeating"."2.0.1" = self.buildNodePackage {
    name = "repeating-2.0.1";
    version = "2.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/repeating/-/repeating-2.0.1.tgz";
      name = "repeating-2.0.1.tgz";
      sha1 = "5214c53a926d3552707527fbab415dbc08d06dda";
    };
    deps = {
      "is-finite-1.0.1" = self.by-version."is-finite"."1.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."replace-ext"."0.0.1" =
    self.by-version."replace-ext"."0.0.1";
  by-version."replace-ext"."0.0.1" = self.buildNodePackage {
    name = "replace-ext-0.0.1";
    version = "0.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/replace-ext/-/replace-ext-0.0.1.tgz";
      name = "replace-ext-0.0.1.tgz";
      sha1 = "29bbd92078a739f0bcce2b4ee41e837953522924";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."replacestream"."^4.0.0" =
    self.by-version."replacestream"."4.0.0";
  by-version."replacestream"."4.0.0" = self.buildNodePackage {
    name = "replacestream-4.0.0";
    version = "4.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/replacestream/-/replacestream-4.0.0.tgz";
      name = "replacestream-4.0.0.tgz";
      sha1 = "ad574af1daf566a83d87ada5ab395e0bdc43e2a4";
    };
    deps = {
      "escape-string-regexp-1.0.5" = self.by-version."escape-string-regexp"."1.0.5";
      "object-assign-3.0.0" = self.by-version."object-assign"."3.0.0";
      "readable-stream-2.1.5" = self.by-version."readable-stream"."2.1.5";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."request"."2" =
    self.by-version."request"."2.74.0";
  by-version."request"."2.74.0" = self.buildNodePackage {
    name = "request-2.74.0";
    version = "2.74.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/request/-/request-2.74.0.tgz";
      name = "request-2.74.0.tgz";
      sha1 = "7693ca768bbb0ea5c8ce08c084a45efa05b892ab";
    };
    deps = {
      "aws-sign2-0.6.0" = self.by-version."aws-sign2"."0.6.0";
      "aws4-1.4.1" = self.by-version."aws4"."1.4.1";
      "bl-1.1.2" = self.by-version."bl"."1.1.2";
      "caseless-0.11.0" = self.by-version."caseless"."0.11.0";
      "combined-stream-1.0.5" = self.by-version."combined-stream"."1.0.5";
      "extend-3.0.0" = self.by-version."extend"."3.0.0";
      "forever-agent-0.6.1" = self.by-version."forever-agent"."0.6.1";
      "form-data-1.0.0-rc4" = self.by-version."form-data"."1.0.0-rc4";
      "har-validator-2.0.6" = self.by-version."har-validator"."2.0.6";
      "hawk-3.1.3" = self.by-version."hawk"."3.1.3";
      "http-signature-1.1.1" = self.by-version."http-signature"."1.1.1";
      "is-typedarray-1.0.0" = self.by-version."is-typedarray"."1.0.0";
      "isstream-0.1.2" = self.by-version."isstream"."0.1.2";
      "json-stringify-safe-5.0.1" = self.by-version."json-stringify-safe"."5.0.1";
      "mime-types-2.1.11" = self.by-version."mime-types"."2.1.11";
      "node-uuid-1.4.7" = self.by-version."node-uuid"."1.4.7";
      "oauth-sign-0.8.2" = self.by-version."oauth-sign"."0.8.2";
      "qs-6.2.1" = self.by-version."qs"."6.2.1";
      "stringstream-0.0.5" = self.by-version."stringstream"."0.0.5";
      "tough-cookie-2.3.1" = self.by-version."tough-cookie"."2.3.1";
      "tunnel-agent-0.4.3" = self.by-version."tunnel-agent"."0.4.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."request"."2.65.0" =
    self.by-version."request"."2.65.0";
  by-version."request"."2.65.0" = self.buildNodePackage {
    name = "request-2.65.0";
    version = "2.65.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/request/-/request-2.65.0.tgz";
      name = "request-2.65.0.tgz";
      sha1 = "cc1a3bc72b96254734fc34296da322f9486ddeba";
    };
    deps = {
      "bl-1.0.3" = self.by-version."bl"."1.0.3";
      "caseless-0.11.0" = self.by-version."caseless"."0.11.0";
      "extend-3.0.0" = self.by-version."extend"."3.0.0";
      "forever-agent-0.6.1" = self.by-version."forever-agent"."0.6.1";
      "form-data-1.0.0-rc4" = self.by-version."form-data"."1.0.0-rc4";
      "json-stringify-safe-5.0.1" = self.by-version."json-stringify-safe"."5.0.1";
      "mime-types-2.1.11" = self.by-version."mime-types"."2.1.11";
      "node-uuid-1.4.7" = self.by-version."node-uuid"."1.4.7";
      "qs-5.2.1" = self.by-version."qs"."5.2.1";
      "tunnel-agent-0.4.3" = self.by-version."tunnel-agent"."0.4.3";
      "tough-cookie-2.2.2" = self.by-version."tough-cookie"."2.2.2";
      "http-signature-0.11.0" = self.by-version."http-signature"."0.11.0";
      "oauth-sign-0.8.2" = self.by-version."oauth-sign"."0.8.2";
      "hawk-3.1.3" = self.by-version."hawk"."3.1.3";
      "aws-sign2-0.6.0" = self.by-version."aws-sign2"."0.6.0";
      "stringstream-0.0.5" = self.by-version."stringstream"."0.0.5";
      "combined-stream-1.0.5" = self.by-version."combined-stream"."1.0.5";
      "isstream-0.1.2" = self.by-version."isstream"."0.1.2";
      "har-validator-2.0.6" = self.by-version."har-validator"."2.0.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."request"."2.x" =
    self.by-version."request"."2.74.0";
  by-spec."request"."^2.55.0" =
    self.by-version."request"."2.74.0";
  by-spec."request"."^2.61.0" =
    self.by-version."request"."2.74.0";
  by-spec."request"."~2.36.0" =
    self.by-version."request"."2.36.0";
  by-version."request"."2.36.0" = self.buildNodePackage {
    name = "request-2.36.0";
    version = "2.36.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/request/-/request-2.36.0.tgz";
      name = "request-2.36.0.tgz";
      sha1 = "28c6c04262c7b9ffdd21b9255374517ee6d943f5";
    };
    deps = {
      "qs-0.6.6" = self.by-version."qs"."0.6.6";
      "json-stringify-safe-5.0.1" = self.by-version."json-stringify-safe"."5.0.1";
      "mime-1.2.11" = self.by-version."mime"."1.2.11";
      "forever-agent-0.5.2" = self.by-version."forever-agent"."0.5.2";
      "node-uuid-1.4.7" = self.by-version."node-uuid"."1.4.7";
    };
    optionalDependencies = {
      "tough-cookie-2.3.1" = self.by-version."tough-cookie"."2.3.1";
      "form-data-0.1.4" = self.by-version."form-data"."0.1.4";
      "tunnel-agent-0.4.3" = self.by-version."tunnel-agent"."0.4.3";
      "http-signature-0.10.1" = self.by-version."http-signature"."0.10.1";
      "oauth-sign-0.3.0" = self.by-version."oauth-sign"."0.3.0";
      "hawk-1.0.0" = self.by-version."hawk"."1.0.0";
      "aws-sign2-0.5.0" = self.by-version."aws-sign2"."0.5.0";
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."require-directory"."^2.1.1" =
    self.by-version."require-directory"."2.1.1";
  by-version."require-directory"."2.1.1" = self.buildNodePackage {
    name = "require-directory-2.1.1";
    version = "2.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/require-directory/-/require-directory-2.1.1.tgz";
      name = "require-directory-2.1.1.tgz";
      sha1 = "8c64ad5fd30dab1c976e2344ffe7f792a6a6df42";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."require-main-filename"."^1.0.0" =
    self.by-version."require-main-filename"."1.0.1";
  by-version."require-main-filename"."1.0.1" = self.buildNodePackage {
    name = "require-main-filename-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/require-main-filename/-/require-main-filename-1.0.1.tgz";
      name = "require-main-filename-1.0.1.tgz";
      sha1 = "97f717b69d48784f5f526a6c5aa8ffdda055a4d1";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."require-main-filename"."^1.0.1" =
    self.by-version."require-main-filename"."1.0.1";
  by-spec."requires-port"."1.x.x" =
    self.by-version."requires-port"."1.0.0";
  by-version."requires-port"."1.0.0" = self.buildNodePackage {
    name = "requires-port-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/requires-port/-/requires-port-1.0.0.tgz";
      name = "requires-port-1.0.0.tgz";
      sha1 = "925d2601d39ac485e091cf0da5c6e694dc3dcaff";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."resolve"."^1.1.6" =
    self.by-version."resolve"."1.1.7";
  by-version."resolve"."1.1.7" = self.buildNodePackage {
    name = "resolve-1.1.7";
    version = "1.1.7";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/resolve/-/resolve-1.1.7.tgz";
      name = "resolve-1.1.7.tgz";
      sha1 = "203114d82ad2c5ed9e8e0411b3932875e889e97b";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."resolve"."^1.1.7" =
    self.by-version."resolve"."1.1.7";
  by-spec."resolve"."~1.1.6" =
    self.by-version."resolve"."1.1.7";
  by-spec."resolve-dir"."^0.1.0" =
    self.by-version."resolve-dir"."0.1.1";
  by-version."resolve-dir"."0.1.1" = self.buildNodePackage {
    name = "resolve-dir-0.1.1";
    version = "0.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/resolve-dir/-/resolve-dir-0.1.1.tgz";
      name = "resolve-dir-0.1.1.tgz";
      sha1 = "b219259a5602fac5c5c496ad894a6e8cc430261e";
    };
    deps = {
      "expand-tilde-1.2.2" = self.by-version."expand-tilde"."1.2.2";
      "global-modules-0.2.3" = self.by-version."global-modules"."0.2.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."resolve-url"."~0.2.1" =
    self.by-version."resolve-url"."0.2.1";
  by-version."resolve-url"."0.2.1" = self.buildNodePackage {
    name = "resolve-url-0.2.1";
    version = "0.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/resolve-url/-/resolve-url-0.2.1.tgz";
      name = "resolve-url-0.2.1.tgz";
      sha1 = "2c637fe77c893afd2a663fe21aa9080068e2052a";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."resp-modifier"."^4.0.2" =
    self.by-version."resp-modifier"."4.0.4";
  by-version."resp-modifier"."4.0.4" = self.buildNodePackage {
    name = "resp-modifier-4.0.4";
    version = "4.0.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/resp-modifier/-/resp-modifier-4.0.4.tgz";
      name = "resp-modifier-4.0.4.tgz";
      sha1 = "8d905cc18c408949a554eeb9c99b75ff9cf0a3fa";
    };
    deps = {
      "minimatch-2.0.10" = self.by-version."minimatch"."2.0.10";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."resp-modifier"."^5.0.0" =
    self.by-version."resp-modifier"."5.0.2";
  by-version."resp-modifier"."5.0.2" = self.buildNodePackage {
    name = "resp-modifier-5.0.2";
    version = "5.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/resp-modifier/-/resp-modifier-5.0.2.tgz";
      name = "resp-modifier-5.0.2.tgz";
      sha1 = "bf0aa6dbf28cd0ca0cb6cc490ffb55943a0bf45c";
    };
    deps = {
      "debug-2.2.0" = self.by-version."debug"."2.2.0";
      "minimatch-2.0.10" = self.by-version."minimatch"."2.0.10";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."right-align"."^0.1.1" =
    self.by-version."right-align"."0.1.3";
  by-version."right-align"."0.1.3" = self.buildNodePackage {
    name = "right-align-0.1.3";
    version = "0.1.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/right-align/-/right-align-0.1.3.tgz";
      name = "right-align-0.1.3.tgz";
      sha1 = "61339b722fe6a3515689210d24e14c96148613ef";
    };
    deps = {
      "align-text-0.1.4" = self.by-version."align-text"."0.1.4";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."rimraf"."2" =
    self.by-version."rimraf"."2.5.4";
  by-version."rimraf"."2.5.4" = self.buildNodePackage {
    name = "rimraf-2.5.4";
    version = "2.5.4";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/rimraf/-/rimraf-2.5.4.tgz";
      name = "rimraf-2.5.4.tgz";
      sha1 = "96800093cbf1a0c86bd95b4625467535c29dfa04";
    };
    deps = {
      "glob-7.0.5" = self.by-version."glob"."7.0.5";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."rimraf"."^2.2.6" =
    self.by-version."rimraf"."2.5.4";
  by-spec."rimraf"."^2.2.8" =
    self.by-version."rimraf"."2.5.4";
  by-spec."rimraf"."~2.3.3" =
    self.by-version."rimraf"."2.3.4";
  by-version."rimraf"."2.3.4" = self.buildNodePackage {
    name = "rimraf-2.3.4";
    version = "2.3.4";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/rimraf/-/rimraf-2.3.4.tgz";
      name = "rimraf-2.3.4.tgz";
      sha1 = "82d9bc1b2fcf31e205ac7b28138a025d08e9159a";
    };
    deps = {
      "glob-4.5.3" = self.by-version."glob"."4.5.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "rimraf" = self.by-version."rimraf"."2.3.4";
  by-spec."rimraf"."~2.5.0" =
    self.by-version."rimraf"."2.5.4";
  by-spec."rimraf"."~2.5.1" =
    self.by-version."rimraf"."2.5.4";
  by-spec."run-sequence"."~1.2.2" =
    self.by-version."run-sequence"."1.2.2";
  by-version."run-sequence"."1.2.2" = self.buildNodePackage {
    name = "run-sequence-1.2.2";
    version = "1.2.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/run-sequence/-/run-sequence-1.2.2.tgz";
      name = "run-sequence-1.2.2.tgz";
      sha1 = "5095a0bebe98733b0140bd08dd80ec030ddacdeb";
    };
    deps = {
      "chalk-1.1.3" = self.by-version."chalk"."1.1.3";
      "gulp-util-3.0.7" = self.by-version."gulp-util"."3.0.7";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "run-sequence" = self.by-version."run-sequence"."1.2.2";
  by-spec."sass-graph"."^2.1.1" =
    self.by-version."sass-graph"."2.1.2";
  by-version."sass-graph"."2.1.2" = self.buildNodePackage {
    name = "sass-graph-2.1.2";
    version = "2.1.2";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/sass-graph/-/sass-graph-2.1.2.tgz";
      name = "sass-graph-2.1.2.tgz";
      sha1 = "965104be23e8103cb7e5f710df65935b317da57b";
    };
    deps = {
      "glob-7.0.5" = self.by-version."glob"."7.0.5";
      "lodash-4.15.0" = self.by-version."lodash"."4.15.0";
      "yargs-4.8.1" = self.by-version."yargs"."4.8.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."sax"."0.4.2" =
    self.by-version."sax"."0.4.2";
  by-version."sax"."0.4.2" = self.buildNodePackage {
    name = "sax-0.4.2";
    version = "0.4.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/sax/-/sax-0.4.2.tgz";
      name = "sax-0.4.2.tgz";
      sha1 = "39f3b601733d6bec97105b242a2a40fd6978ac3c";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."sax"."1.1.5" =
    self.by-version."sax"."1.1.5";
  by-version."sax"."1.1.5" = self.buildNodePackage {
    name = "sax-1.1.5";
    version = "1.1.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/sax/-/sax-1.1.5.tgz";
      name = "sax-1.1.5.tgz";
      sha1 = "1da50a8d00cdecd59405659f5ff85349fe773743";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."sax".">=0.6.0" =
    self.by-version."sax"."1.2.1";
  by-version."sax"."1.2.1" = self.buildNodePackage {
    name = "sax-1.2.1";
    version = "1.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/sax/-/sax-1.2.1.tgz";
      name = "sax-1.2.1.tgz";
      sha1 = "7b8e656190b228e81a66aea748480d828cd2d37a";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."sax"."^1.1.4" =
    self.by-version."sax"."1.2.1";
  by-spec."sax"."~1.2.1" =
    self.by-version."sax"."1.2.1";
  by-spec."seek-bzip"."^1.0.3" =
    self.by-version."seek-bzip"."1.0.5";
  by-version."seek-bzip"."1.0.5" = self.buildNodePackage {
    name = "seek-bzip-1.0.5";
    version = "1.0.5";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/seek-bzip/-/seek-bzip-1.0.5.tgz";
      name = "seek-bzip-1.0.5.tgz";
      sha1 = "cfe917cb3d274bcffac792758af53173eb1fabdc";
    };
    deps = {
      "commander-2.8.1" = self.by-version."commander"."2.8.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."semver"."2 || 3 || 4 || 5" =
    self.by-version."semver"."5.3.0";
  by-version."semver"."5.3.0" = self.buildNodePackage {
    name = "semver-5.3.0";
    version = "5.3.0";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/semver/-/semver-5.3.0.tgz";
      name = "semver-5.3.0.tgz";
      sha1 = "9b2ce5d3de02d17c6012ad326aa6b4d0cf54f94f";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."semver"."2.x || 3.x || 4 || 5" =
    self.by-version."semver"."5.3.0";
  by-spec."semver"."^4.0.3" =
    self.by-version."semver"."4.3.6";
  by-version."semver"."4.3.6" = self.buildNodePackage {
    name = "semver-4.3.6";
    version = "4.3.6";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/semver/-/semver-4.3.6.tgz";
      name = "semver-4.3.6.tgz";
      sha1 = "300bc6e0e86374f7ba61068b5b1ecd57fc6532da";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."semver"."^4.1.0" =
    self.by-version."semver"."4.3.6";
  by-spec."semver"."^4.2.0" =
    self.by-version."semver"."4.3.6";
  by-spec."semver"."^5.3.0" =
    self.by-version."semver"."5.3.0";
  by-spec."semver"."~5.2.0" =
    self.by-version."semver"."5.2.0";
  by-version."semver"."5.2.0" = self.buildNodePackage {
    name = "semver-5.2.0";
    version = "5.2.0";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/semver/-/semver-5.2.0.tgz";
      name = "semver-5.2.0.tgz";
      sha1 = "281995b80c1448209415ddbc4cf50c269cef55c5";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."semver-greatest-satisfied-range"."^1.0.0" =
    self.by-version."semver-greatest-satisfied-range"."1.0.0";
  by-version."semver-greatest-satisfied-range"."1.0.0" = self.buildNodePackage {
    name = "semver-greatest-satisfied-range-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/semver-greatest-satisfied-range/-/semver-greatest-satisfied-range-1.0.0.tgz";
      name = "semver-greatest-satisfied-range-1.0.0.tgz";
      sha1 = "4fb441e2a8d26c40b598327557318de272a558a0";
    };
    deps = {
      "semver-4.3.6" = self.by-version."semver"."4.3.6";
      "semver-regex-1.0.0" = self.by-version."semver-regex"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."semver-regex"."^1.0.0" =
    self.by-version."semver-regex"."1.0.0";
  by-version."semver-regex"."1.0.0" = self.buildNodePackage {
    name = "semver-regex-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/semver-regex/-/semver-regex-1.0.0.tgz";
      name = "semver-regex-1.0.0.tgz";
      sha1 = "92a4969065f9c70c694753d55248fc68f8f652c9";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."semver-truncate"."^1.0.0" =
    self.by-version."semver-truncate"."1.1.2";
  by-version."semver-truncate"."1.1.2" = self.buildNodePackage {
    name = "semver-truncate-1.1.2";
    version = "1.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/semver-truncate/-/semver-truncate-1.1.2.tgz";
      name = "semver-truncate-1.1.2.tgz";
      sha1 = "57f41de69707a62709a7e0104ba2117109ea47e8";
    };
    deps = {
      "semver-5.3.0" = self.by-version."semver"."5.3.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."send"."0.14.1" =
    self.by-version."send"."0.14.1";
  by-version."send"."0.14.1" = self.buildNodePackage {
    name = "send-0.14.1";
    version = "0.14.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/send/-/send-0.14.1.tgz";
      name = "send-0.14.1.tgz";
      sha1 = "a954984325392f51532a7760760e459598c89f7a";
    };
    deps = {
      "debug-2.2.0" = self.by-version."debug"."2.2.0";
      "depd-1.1.0" = self.by-version."depd"."1.1.0";
      "destroy-1.0.4" = self.by-version."destroy"."1.0.4";
      "encodeurl-1.0.1" = self.by-version."encodeurl"."1.0.1";
      "escape-html-1.0.3" = self.by-version."escape-html"."1.0.3";
      "etag-1.7.0" = self.by-version."etag"."1.7.0";
      "fresh-0.3.0" = self.by-version."fresh"."0.3.0";
      "http-errors-1.5.0" = self.by-version."http-errors"."1.5.0";
      "mime-1.3.4" = self.by-version."mime"."1.3.4";
      "ms-0.7.1" = self.by-version."ms"."0.7.1";
      "on-finished-2.3.0" = self.by-version."on-finished"."2.3.0";
      "range-parser-1.2.0" = self.by-version."range-parser"."1.2.0";
      "statuses-1.3.0" = self.by-version."statuses"."1.3.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."sentence-case"."^1.1.1" =
    self.by-version."sentence-case"."1.1.3";
  by-version."sentence-case"."1.1.3" = self.buildNodePackage {
    name = "sentence-case-1.1.3";
    version = "1.1.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/sentence-case/-/sentence-case-1.1.3.tgz";
      name = "sentence-case-1.1.3.tgz";
      sha1 = "8034aafc2145772d3abe1509aa42c9e1042dc139";
    };
    deps = {
      "lower-case-1.1.3" = self.by-version."lower-case"."1.1.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."sentence-case"."^1.1.2" =
    self.by-version."sentence-case"."1.1.3";
  by-spec."sequencify"."~0.0.7" =
    self.by-version."sequencify"."0.0.7";
  by-version."sequencify"."0.0.7" = self.buildNodePackage {
    name = "sequencify-0.0.7";
    version = "0.0.7";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/sequencify/-/sequencify-0.0.7.tgz";
      name = "sequencify-0.0.7.tgz";
      sha1 = "90cff19d02e07027fd767f5ead3e7b95d1e7380c";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."serve-index"."^1.7.0" =
    self.by-version."serve-index"."1.8.0";
  by-version."serve-index"."1.8.0" = self.buildNodePackage {
    name = "serve-index-1.8.0";
    version = "1.8.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/serve-index/-/serve-index-1.8.0.tgz";
      name = "serve-index-1.8.0.tgz";
      sha1 = "7c5d96c13fb131101f93c1c5774f8516a1e78d3b";
    };
    deps = {
      "accepts-1.3.3" = self.by-version."accepts"."1.3.3";
      "batch-0.5.3" = self.by-version."batch"."0.5.3";
      "debug-2.2.0" = self.by-version."debug"."2.2.0";
      "escape-html-1.0.3" = self.by-version."escape-html"."1.0.3";
      "http-errors-1.5.0" = self.by-version."http-errors"."1.5.0";
      "mime-types-2.1.11" = self.by-version."mime-types"."2.1.11";
      "parseurl-1.3.1" = self.by-version."parseurl"."1.3.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."serve-static"."^1.10.0" =
    self.by-version."serve-static"."1.11.1";
  by-version."serve-static"."1.11.1" = self.buildNodePackage {
    name = "serve-static-1.11.1";
    version = "1.11.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/serve-static/-/serve-static-1.11.1.tgz";
      name = "serve-static-1.11.1.tgz";
      sha1 = "d6cce7693505f733c759de57befc1af76c0f0805";
    };
    deps = {
      "encodeurl-1.0.1" = self.by-version."encodeurl"."1.0.1";
      "escape-html-1.0.3" = self.by-version."escape-html"."1.0.3";
      "parseurl-1.3.1" = self.by-version."parseurl"."1.3.1";
      "send-0.14.1" = self.by-version."send"."0.14.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."set-blocking"."^2.0.0" =
    self.by-version."set-blocking"."2.0.0";
  by-version."set-blocking"."2.0.0" = self.buildNodePackage {
    name = "set-blocking-2.0.0";
    version = "2.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/set-blocking/-/set-blocking-2.0.0.tgz";
      name = "set-blocking-2.0.0.tgz";
      sha1 = "045f9782d011ae9a6803ddd382b24392b3d890f7";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."set-blocking"."~2.0.0" =
    self.by-version."set-blocking"."2.0.0";
  by-spec."set-immediate-shim"."^1.0.0" =
    self.by-version."set-immediate-shim"."1.0.1";
  by-version."set-immediate-shim"."1.0.1" = self.buildNodePackage {
    name = "set-immediate-shim-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/set-immediate-shim/-/set-immediate-shim-1.0.1.tgz";
      name = "set-immediate-shim-1.0.1.tgz";
      sha1 = "4b2b1b27eb808a9f8dcc481a58e5e56f599f3f61";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."set-immediate-shim"."^1.0.1" =
    self.by-version."set-immediate-shim"."1.0.1";
  by-spec."setprototypeof"."1.0.1" =
    self.by-version."setprototypeof"."1.0.1";
  by-version."setprototypeof"."1.0.1" = self.buildNodePackage {
    name = "setprototypeof-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/setprototypeof/-/setprototypeof-1.0.1.tgz";
      name = "setprototypeof-1.0.1.tgz";
      sha1 = "52009b27888c4dc48f591949c0a8275834c1ca7e";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."shebang-regex"."^1.0.0" =
    self.by-version."shebang-regex"."1.0.0";
  by-version."shebang-regex"."1.0.0" = self.buildNodePackage {
    name = "shebang-regex-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/shebang-regex/-/shebang-regex-1.0.0.tgz";
      name = "shebang-regex-1.0.0.tgz";
      sha1 = "da42f49740c0b42db2ca9728571cb190c98efea3";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."sigmund"."~1.0.0" =
    self.by-version."sigmund"."1.0.1";
  by-version."sigmund"."1.0.1" = self.buildNodePackage {
    name = "sigmund-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/sigmund/-/sigmund-1.0.1.tgz";
      name = "sigmund-1.0.1.tgz";
      sha1 = "3ff21f198cad2175f9f3b781853fd94d0d19b590";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."signal-exit"."^3.0.0" =
    self.by-version."signal-exit"."3.0.0";
  by-version."signal-exit"."3.0.0" = self.buildNodePackage {
    name = "signal-exit-3.0.0";
    version = "3.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/signal-exit/-/signal-exit-3.0.0.tgz";
      name = "signal-exit-3.0.0.tgz";
      sha1 = "3c0543b65d7b4fbc60b6cd94593d9bf436739be8";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."simplesmtp"."~0.2 || ~0.3.30" =
    self.by-version."simplesmtp"."0.3.35";
  by-version."simplesmtp"."0.3.35" = self.buildNodePackage {
    name = "simplesmtp-0.3.35";
    version = "0.3.35";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/simplesmtp/-/simplesmtp-0.3.35.tgz";
      name = "simplesmtp-0.3.35.tgz";
      sha1 = "017b1eb8b26317ac36d2a2a8a932631880736a03";
    };
    deps = {
      "rai-0.1.12" = self.by-version."rai"."0.1.12";
      "xoauth2-0.1.8" = self.by-version."xoauth2"."0.1.8";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."simplesmtp"."~0.3.30" =
    self.by-version."simplesmtp"."0.3.35";
  by-spec."siphon-media-query"."~1.0.0" =
    self.by-version."siphon-media-query"."1.0.0";
  by-version."siphon-media-query"."1.0.0" = self.buildNodePackage {
    name = "siphon-media-query-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/siphon-media-query/-/siphon-media-query-1.0.0.tgz";
      name = "siphon-media-query-1.0.0.tgz";
      sha1 = "1973d7260d5e92a322fc27372f1ca224059e53c8";
    };
    deps = {
      "css-parse-2.0.0" = self.by-version."css-parse"."2.0.0";
      "css-stringify-2.0.0" = self.by-version."css-stringify"."2.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "siphon-media-query" = self.by-version."siphon-media-query"."1.0.0";
  by-spec."slash"."^1.0.0" =
    self.by-version."slash"."1.0.0";
  by-version."slash"."1.0.0" = self.buildNodePackage {
    name = "slash-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/slash/-/slash-1.0.0.tgz";
      name = "slash-1.0.0.tgz";
      sha1 = "c41f2f6c39fc16d1cd17ad4b5d896114ae470d55";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."slick"."^1.12.1" =
    self.by-version."slick"."1.12.2";
  by-version."slick"."1.12.2" = self.buildNodePackage {
    name = "slick-1.12.2";
    version = "1.12.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/slick/-/slick-1.12.2.tgz";
      name = "slick-1.12.2.tgz";
      sha1 = "bd048ddb74de7d1ca6915faa4a57570b3550c2d7";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."snake-case"."^1.1.0" =
    self.by-version."snake-case"."1.1.2";
  by-version."snake-case"."1.1.2" = self.buildNodePackage {
    name = "snake-case-1.1.2";
    version = "1.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/snake-case/-/snake-case-1.1.2.tgz";
      name = "snake-case-1.1.2.tgz";
      sha1 = "0c2f25e305158d9a18d3d977066187fef8a5a66a";
    };
    deps = {
      "sentence-case-1.1.3" = self.by-version."sentence-case"."1.1.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."sntp"."0.2.x" =
    self.by-version."sntp"."0.2.4";
  by-version."sntp"."0.2.4" = self.buildNodePackage {
    name = "sntp-0.2.4";
    version = "0.2.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/sntp/-/sntp-0.2.4.tgz";
      name = "sntp-0.2.4.tgz";
      sha1 = "fb885f18b0f3aad189f824862536bceeec750900";
    };
    deps = {
      "hoek-0.9.1" = self.by-version."hoek"."0.9.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."sntp"."1.x.x" =
    self.by-version."sntp"."1.0.9";
  by-version."sntp"."1.0.9" = self.buildNodePackage {
    name = "sntp-1.0.9";
    version = "1.0.9";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/sntp/-/sntp-1.0.9.tgz";
      name = "sntp-1.0.9.tgz";
      sha1 = "6541184cc90aeea6c6e7b35e2659082443c66198";
    };
    deps = {
      "hoek-2.16.3" = self.by-version."hoek"."2.16.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."socket.io"."1.4.5" =
    self.by-version."socket.io"."1.4.5";
  by-version."socket.io"."1.4.5" = self.buildNodePackage {
    name = "socket.io-1.4.5";
    version = "1.4.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/socket.io/-/socket.io-1.4.5.tgz";
      name = "socket.io-1.4.5.tgz";
      sha1 = "f202f49eeb9cf7cf6c0971ad75d8d96d451ea4f7";
    };
    deps = {
      "engine.io-1.6.8" = self.by-version."engine.io"."1.6.8";
      "socket.io-parser-2.2.6" = self.by-version."socket.io-parser"."2.2.6";
      "socket.io-client-1.4.5" = self.by-version."socket.io-client"."1.4.5";
      "socket.io-adapter-0.4.0" = self.by-version."socket.io-adapter"."0.4.0";
      "has-binary-0.1.7" = self.by-version."has-binary"."0.1.7";
      "debug-2.2.0" = self.by-version."debug"."2.2.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."socket.io-adapter"."0.4.0" =
    self.by-version."socket.io-adapter"."0.4.0";
  by-version."socket.io-adapter"."0.4.0" = self.buildNodePackage {
    name = "socket.io-adapter-0.4.0";
    version = "0.4.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/socket.io-adapter/-/socket.io-adapter-0.4.0.tgz";
      name = "socket.io-adapter-0.4.0.tgz";
      sha1 = "fb9f82ab1aa65290bf72c3657955b930a991a24f";
    };
    deps = {
      "debug-2.2.0" = self.by-version."debug"."2.2.0";
      "socket.io-parser-2.2.2" = self.by-version."socket.io-parser"."2.2.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."socket.io-client"."1.4.5" =
    self.by-version."socket.io-client"."1.4.5";
  by-version."socket.io-client"."1.4.5" = self.buildNodePackage {
    name = "socket.io-client-1.4.5";
    version = "1.4.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/socket.io-client/-/socket.io-client-1.4.5.tgz";
      name = "socket.io-client-1.4.5.tgz";
      sha1 = "400d630c31e7c9579e45173f977e4f5bd8dc7d2e";
    };
    deps = {
      "debug-2.2.0" = self.by-version."debug"."2.2.0";
      "engine.io-client-1.6.8" = self.by-version."engine.io-client"."1.6.8";
      "component-bind-1.0.0" = self.by-version."component-bind"."1.0.0";
      "component-emitter-1.2.0" = self.by-version."component-emitter"."1.2.0";
      "object-component-0.0.3" = self.by-version."object-component"."0.0.3";
      "socket.io-parser-2.2.6" = self.by-version."socket.io-parser"."2.2.6";
      "has-binary-0.1.7" = self.by-version."has-binary"."0.1.7";
      "indexof-0.0.1" = self.by-version."indexof"."0.0.1";
      "parseuri-0.0.4" = self.by-version."parseuri"."0.0.4";
      "to-array-0.1.4" = self.by-version."to-array"."0.1.4";
      "backo2-1.0.2" = self.by-version."backo2"."1.0.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."socket.io-parser"."2.2.2" =
    self.by-version."socket.io-parser"."2.2.2";
  by-version."socket.io-parser"."2.2.2" = self.buildNodePackage {
    name = "socket.io-parser-2.2.2";
    version = "2.2.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/socket.io-parser/-/socket.io-parser-2.2.2.tgz";
      name = "socket.io-parser-2.2.2.tgz";
      sha1 = "3d7af6b64497e956b7d9fe775f999716027f9417";
    };
    deps = {
      "debug-0.7.4" = self.by-version."debug"."0.7.4";
      "json3-3.2.6" = self.by-version."json3"."3.2.6";
      "component-emitter-1.1.2" = self.by-version."component-emitter"."1.1.2";
      "isarray-0.0.1" = self.by-version."isarray"."0.0.1";
      "benchmark-1.0.0" = self.by-version."benchmark"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."socket.io-parser"."2.2.6" =
    self.by-version."socket.io-parser"."2.2.6";
  by-version."socket.io-parser"."2.2.6" = self.buildNodePackage {
    name = "socket.io-parser-2.2.6";
    version = "2.2.6";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/socket.io-parser/-/socket.io-parser-2.2.6.tgz";
      name = "socket.io-parser-2.2.6.tgz";
      sha1 = "38dfd61df50dcf8ab1d9e2091322bf902ba28b99";
    };
    deps = {
      "debug-2.2.0" = self.by-version."debug"."2.2.0";
      "json3-3.3.2" = self.by-version."json3"."3.3.2";
      "component-emitter-1.1.2" = self.by-version."component-emitter"."1.1.2";
      "isarray-0.0.1" = self.by-version."isarray"."0.0.1";
      "benchmark-1.0.0" = self.by-version."benchmark"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."source-map"."0.1.32" =
    self.by-version."source-map"."0.1.32";
  by-version."source-map"."0.1.32" = self.buildNodePackage {
    name = "source-map-0.1.32";
    version = "0.1.32";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/source-map/-/source-map-0.1.32.tgz";
      name = "source-map-0.1.32.tgz";
      sha1 = "c8b6c167797ba4740a8ea33252162ff08591b266";
    };
    deps = {
      "amdefine-1.0.0" = self.by-version."amdefine"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."source-map"."0.1.34" =
    self.by-version."source-map"."0.1.34";
  by-version."source-map"."0.1.34" = self.buildNodePackage {
    name = "source-map-0.1.34";
    version = "0.1.34";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/source-map/-/source-map-0.1.34.tgz";
      name = "source-map-0.1.34.tgz";
      sha1 = "a7cfe89aec7b1682c3b198d0acfb47d7d090566b";
    };
    deps = {
      "amdefine-1.0.0" = self.by-version."amdefine"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."source-map".">=0.1.43 <0.2" =
    self.by-version."source-map"."0.1.43";
  by-version."source-map"."0.1.43" = self.buildNodePackage {
    name = "source-map-0.1.43";
    version = "0.1.43";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/source-map/-/source-map-0.1.43.tgz";
      name = "source-map-0.1.43.tgz";
      sha1 = "c24bc146ca517c1471f5dacbe2571b2b7f9e3346";
    };
    deps = {
      "amdefine-1.0.0" = self.by-version."amdefine"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."source-map"."^0.1.38" =
    self.by-version."source-map"."0.1.43";
  by-spec."source-map"."^0.4.4" =
    self.by-version."source-map"."0.4.4";
  by-version."source-map"."0.4.4" = self.buildNodePackage {
    name = "source-map-0.4.4";
    version = "0.4.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/source-map/-/source-map-0.4.4.tgz";
      name = "source-map-0.4.4.tgz";
      sha1 = "eba4f5da9c0dc999de68032d8b4f76173652036b";
    };
    deps = {
      "amdefine-1.0.0" = self.by-version."amdefine"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."source-map"."^0.5.0" =
    self.by-version."source-map"."0.5.6";
  by-version."source-map"."0.5.6" = self.buildNodePackage {
    name = "source-map-0.5.6";
    version = "0.5.6";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/source-map/-/source-map-0.5.6.tgz";
      name = "source-map-0.5.6.tgz";
      sha1 = "75ce38f52bf0733c5a7f0c118d81334a2bb5f412";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."source-map"."^0.5.1" =
    self.by-version."source-map"."0.5.6";
  by-spec."source-map"."^0.5.3" =
    self.by-version."source-map"."0.5.6";
  by-spec."source-map"."~0.2.0" =
    self.by-version."source-map"."0.2.0";
  by-version."source-map"."0.2.0" = self.buildNodePackage {
    name = "source-map-0.2.0";
    version = "0.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/source-map/-/source-map-0.2.0.tgz";
      name = "source-map-0.2.0.tgz";
      sha1 = "dab73fbcfc2ba819b4de03bd6f6eaa48164b3f9d";
    };
    deps = {
      "amdefine-1.0.0" = self.by-version."amdefine"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."source-map"."~0.5.1" =
    self.by-version."source-map"."0.5.6";
  by-spec."source-map-resolve"."^0.3.0" =
    self.by-version."source-map-resolve"."0.3.1";
  by-version."source-map-resolve"."0.3.1" = self.buildNodePackage {
    name = "source-map-resolve-0.3.1";
    version = "0.3.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/source-map-resolve/-/source-map-resolve-0.3.1.tgz";
      name = "source-map-resolve-0.3.1.tgz";
      sha1 = "610f6122a445b8dd51535a2a71b783dfc1248761";
    };
    deps = {
      "source-map-url-0.3.0" = self.by-version."source-map-url"."0.3.0";
      "atob-1.1.3" = self.by-version."atob"."1.1.3";
      "urix-0.1.0" = self.by-version."urix"."0.1.0";
      "resolve-url-0.2.1" = self.by-version."resolve-url"."0.2.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."source-map-support"."^0.2.10" =
    self.by-version."source-map-support"."0.2.10";
  by-version."source-map-support"."0.2.10" = self.buildNodePackage {
    name = "source-map-support-0.2.10";
    version = "0.2.10";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/source-map-support/-/source-map-support-0.2.10.tgz";
      name = "source-map-support-0.2.10.tgz";
      sha1 = "ea5a3900a1c1cb25096a0ae8cc5c2b4b10ded3dc";
    };
    deps = {
      "source-map-0.1.32" = self.by-version."source-map"."0.1.32";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."source-map-url"."~0.3.0" =
    self.by-version."source-map-url"."0.3.0";
  by-version."source-map-url"."0.3.0" = self.buildNodePackage {
    name = "source-map-url-0.3.0";
    version = "0.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/source-map-url/-/source-map-url-0.3.0.tgz";
      name = "source-map-url-0.3.0.tgz";
      sha1 = "7ecaf13b57bcd09da8a40c5d269db33799d4aaf9";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."sparkles"."^1.0.0" =
    self.by-version."sparkles"."1.0.0";
  by-version."sparkles"."1.0.0" = self.buildNodePackage {
    name = "sparkles-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/sparkles/-/sparkles-1.0.0.tgz";
      name = "sparkles-1.0.0.tgz";
      sha1 = "1acbbfb592436d10bbe8f785b7cc6f82815012c3";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."spdx-correct"."~1.0.0" =
    self.by-version."spdx-correct"."1.0.2";
  by-version."spdx-correct"."1.0.2" = self.buildNodePackage {
    name = "spdx-correct-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/spdx-correct/-/spdx-correct-1.0.2.tgz";
      name = "spdx-correct-1.0.2.tgz";
      sha1 = "4b3073d933ff51f3912f03ac5519498a4150db40";
    };
    deps = {
      "spdx-license-ids-1.2.2" = self.by-version."spdx-license-ids"."1.2.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."spdx-exceptions"."^1.0.4" =
    self.by-version."spdx-exceptions"."1.0.5";
  by-version."spdx-exceptions"."1.0.5" = self.buildNodePackage {
    name = "spdx-exceptions-1.0.5";
    version = "1.0.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/spdx-exceptions/-/spdx-exceptions-1.0.5.tgz";
      name = "spdx-exceptions-1.0.5.tgz";
      sha1 = "9d21ac4da4bdb71d060fb74e5a67531d032cbba6";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."spdx-expression-parse"."~1.0.0" =
    self.by-version."spdx-expression-parse"."1.0.2";
  by-version."spdx-expression-parse"."1.0.2" = self.buildNodePackage {
    name = "spdx-expression-parse-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/spdx-expression-parse/-/spdx-expression-parse-1.0.2.tgz";
      name = "spdx-expression-parse-1.0.2.tgz";
      sha1 = "d52b14b5e9670771440af225bcb563122ac452f6";
    };
    deps = {
      "spdx-exceptions-1.0.5" = self.by-version."spdx-exceptions"."1.0.5";
      "spdx-license-ids-1.2.2" = self.by-version."spdx-license-ids"."1.2.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."spdx-license-ids"."^1.0.0" =
    self.by-version."spdx-license-ids"."1.2.2";
  by-version."spdx-license-ids"."1.2.2" = self.buildNodePackage {
    name = "spdx-license-ids-1.2.2";
    version = "1.2.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/spdx-license-ids/-/spdx-license-ids-1.2.2.tgz";
      name = "spdx-license-ids-1.2.2.tgz";
      sha1 = "c9df7a3424594ade6bd11900d596696dc06bac57";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."spdx-license-ids"."^1.0.2" =
    self.by-version."spdx-license-ids"."1.2.2";
  by-spec."split"."0.2" =
    self.by-version."split"."0.2.10";
  by-version."split"."0.2.10" = self.buildNodePackage {
    name = "split-0.2.10";
    version = "0.2.10";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/split/-/split-0.2.10.tgz";
      name = "split-0.2.10.tgz";
      sha1 = "67097c601d697ce1368f418f06cd201cf0521a57";
    };
    deps = {
      "through-2.3.8" = self.by-version."through"."2.3.8";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."split2"."^0.2.1" =
    self.by-version."split2"."0.2.1";
  by-version."split2"."0.2.1" = self.buildNodePackage {
    name = "split2-0.2.1";
    version = "0.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/split2/-/split2-0.2.1.tgz";
      name = "split2-0.2.1.tgz";
      sha1 = "02ddac9adc03ec0bb78c1282ec079ca6e85ae900";
    };
    deps = {
      "through2-0.6.5" = self.by-version."through2"."0.6.5";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."sprintf-js"."~1.0.2" =
    self.by-version."sprintf-js"."1.0.3";
  by-version."sprintf-js"."1.0.3" = self.buildNodePackage {
    name = "sprintf-js-1.0.3";
    version = "1.0.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/sprintf-js/-/sprintf-js-1.0.3.tgz";
      name = "sprintf-js-1.0.3.tgz";
      sha1 = "04e6926f662895354f3dd015203633b857297e2c";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."squeak"."^1.0.0" =
    self.by-version."squeak"."1.3.0";
  by-version."squeak"."1.3.0" = self.buildNodePackage {
    name = "squeak-1.3.0";
    version = "1.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/squeak/-/squeak-1.3.0.tgz";
      name = "squeak-1.3.0.tgz";
      sha1 = "33045037b64388b567674b84322a6521073916c3";
    };
    deps = {
      "chalk-1.1.3" = self.by-version."chalk"."1.1.3";
      "console-stream-0.1.1" = self.by-version."console-stream"."0.1.1";
      "lpad-align-1.1.0" = self.by-version."lpad-align"."1.1.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."sshpk"."^1.7.0" =
    self.by-version."sshpk"."1.9.2";
  by-version."sshpk"."1.9.2" = self.buildNodePackage {
    name = "sshpk-1.9.2";
    version = "1.9.2";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/sshpk/-/sshpk-1.9.2.tgz";
      name = "sshpk-1.9.2.tgz";
      sha1 = "3b41351bbad5c34ddf4bd8119937efee31a46765";
    };
    deps = {
      "asn1-0.2.3" = self.by-version."asn1"."0.2.3";
      "assert-plus-1.0.0" = self.by-version."assert-plus"."1.0.0";
      "dashdash-1.14.0" = self.by-version."dashdash"."1.14.0";
      "getpass-0.1.6" = self.by-version."getpass"."0.1.6";
    };
    optionalDependencies = {
      "jsbn-0.1.0" = self.by-version."jsbn"."0.1.0";
      "tweetnacl-0.13.3" = self.by-version."tweetnacl"."0.13.3";
      "jodid25519-1.0.2" = self.by-version."jodid25519"."1.0.2";
      "ecc-jsbn-0.1.1" = self.by-version."ecc-jsbn"."0.1.1";
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."stack-trace"."0.0.9" =
    self.by-version."stack-trace"."0.0.9";
  by-version."stack-trace"."0.0.9" = self.buildNodePackage {
    name = "stack-trace-0.0.9";
    version = "0.0.9";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/stack-trace/-/stack-trace-0.0.9.tgz";
      name = "stack-trace-0.0.9.tgz";
      sha1 = "a8f6eaeca90674c333e7c43953f275b451510695";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."stat-mode"."^0.2.0" =
    self.by-version."stat-mode"."0.2.1";
  by-version."stat-mode"."0.2.1" = self.buildNodePackage {
    name = "stat-mode-0.2.1";
    version = "0.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/stat-mode/-/stat-mode-0.2.1.tgz";
      name = "stat-mode-0.2.1.tgz";
      sha1 = "d714e08a4ed157089c1340f76fee54046c8242d6";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."statuses".">= 1.3.0 < 2" =
    self.by-version."statuses"."1.3.0";
  by-version."statuses"."1.3.0" = self.buildNodePackage {
    name = "statuses-1.3.0";
    version = "1.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/statuses/-/statuses-1.3.0.tgz";
      name = "statuses-1.3.0.tgz";
      sha1 = "8e55758cb20e7682c1f4fce8dcab30bf01d1e07a";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."statuses"."~1.3.0" =
    self.by-version."statuses"."1.3.0";
  by-spec."stream-combiner"."*" =
    self.by-version."stream-combiner"."0.2.2";
  by-version."stream-combiner"."0.2.2" = self.buildNodePackage {
    name = "stream-combiner-0.2.2";
    version = "0.2.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/stream-combiner/-/stream-combiner-0.2.2.tgz";
      name = "stream-combiner-0.2.2.tgz";
      sha1 = "aec8cbac177b56b6f4fa479ced8c1912cee52858";
    };
    deps = {
      "duplexer-0.1.1" = self.by-version."duplexer"."0.1.1";
      "through-2.3.8" = self.by-version."through"."2.3.8";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."stream-combiner"."~0.0.4" =
    self.by-version."stream-combiner"."0.0.4";
  by-version."stream-combiner"."0.0.4" = self.buildNodePackage {
    name = "stream-combiner-0.0.4";
    version = "0.0.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/stream-combiner/-/stream-combiner-0.0.4.tgz";
      name = "stream-combiner-0.0.4.tgz";
      sha1 = "4d5e433c185261dde623ca3f44c586bcf5c4ad14";
    };
    deps = {
      "duplexer-0.1.1" = self.by-version."duplexer"."0.1.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."stream-combiner2"."^1.1.1" =
    self.by-version."stream-combiner2"."1.1.1";
  by-version."stream-combiner2"."1.1.1" = self.buildNodePackage {
    name = "stream-combiner2-1.1.1";
    version = "1.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/stream-combiner2/-/stream-combiner2-1.1.1.tgz";
      name = "stream-combiner2-1.1.1.tgz";
      sha1 = "fb4d8a1420ea362764e21ad4780397bebcb41cbe";
    };
    deps = {
      "duplexer2-0.1.4" = self.by-version."duplexer2"."0.1.4";
      "readable-stream-2.1.5" = self.by-version."readable-stream"."2.1.5";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."stream-consume"."~0.1.0" =
    self.by-version."stream-consume"."0.1.0";
  by-version."stream-consume"."0.1.0" = self.buildNodePackage {
    name = "stream-consume-0.1.0";
    version = "0.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/stream-consume/-/stream-consume-0.1.0.tgz";
      name = "stream-consume-0.1.0.tgz";
      sha1 = "a41ead1a6d6081ceb79f65b061901b6d8f3d1d0f";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."stream-shift"."^1.0.0" =
    self.by-version."stream-shift"."1.0.0";
  by-version."stream-shift"."1.0.0" = self.buildNodePackage {
    name = "stream-shift-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/stream-shift/-/stream-shift-1.0.0.tgz";
      name = "stream-shift-1.0.0.tgz";
      sha1 = "d5c752825e5367e786f78e18e445ea223a155952";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."stream-throttle"."^0.1.3" =
    self.by-version."stream-throttle"."0.1.3";
  by-version."stream-throttle"."0.1.3" = self.buildNodePackage {
    name = "stream-throttle-0.1.3";
    version = "0.1.3";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/stream-throttle/-/stream-throttle-0.1.3.tgz";
      name = "stream-throttle-0.1.3.tgz";
      sha1 = "add57c8d7cc73a81630d31cd55d3961cfafba9c3";
    };
    deps = {
      "commander-2.9.0" = self.by-version."commander"."2.9.0";
      "limiter-1.1.0" = self.by-version."limiter"."1.1.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."strict-uri-encode"."^1.0.0" =
    self.by-version."strict-uri-encode"."1.1.0";
  by-version."strict-uri-encode"."1.1.0" = self.buildNodePackage {
    name = "strict-uri-encode-1.1.0";
    version = "1.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/strict-uri-encode/-/strict-uri-encode-1.1.0.tgz";
      name = "strict-uri-encode-1.1.0.tgz";
      sha1 = "279b225df1d582b1f54e65addd4352e18faa0713";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."string-width"."^1.0.1" =
    self.by-version."string-width"."1.0.2";
  by-version."string-width"."1.0.2" = self.buildNodePackage {
    name = "string-width-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/string-width/-/string-width-1.0.2.tgz";
      name = "string-width-1.0.2.tgz";
      sha1 = "118bdf5b8cdc51a2a7e70d211e07e2b0b9b107d3";
    };
    deps = {
      "code-point-at-1.0.0" = self.by-version."code-point-at"."1.0.0";
      "is-fullwidth-code-point-1.0.0" = self.by-version."is-fullwidth-code-point"."1.0.0";
      "strip-ansi-3.0.1" = self.by-version."strip-ansi"."3.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."string_decoder"."~0.10.x" =
    self.by-version."string_decoder"."0.10.31";
  by-version."string_decoder"."0.10.31" = self.buildNodePackage {
    name = "string_decoder-0.10.31";
    version = "0.10.31";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/string_decoder/-/string_decoder-0.10.31.tgz";
      name = "string_decoder-0.10.31.tgz";
      sha1 = "62e203bc41766c6c28c9fc84301dab1c5310fa94";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."stringstream"."~0.0.4" =
    self.by-version."stringstream"."0.0.5";
  by-version."stringstream"."0.0.5" = self.buildNodePackage {
    name = "stringstream-0.0.5";
    version = "0.0.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/stringstream/-/stringstream-0.0.5.tgz";
      name = "stringstream-0.0.5.tgz";
      sha1 = "4e484cd4de5a0bbbee18e46307710a8a81621878";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."strip-ansi"."^0.3.0" =
    self.by-version."strip-ansi"."0.3.0";
  by-version."strip-ansi"."0.3.0" = self.buildNodePackage {
    name = "strip-ansi-0.3.0";
    version = "0.3.0";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/strip-ansi/-/strip-ansi-0.3.0.tgz";
      name = "strip-ansi-0.3.0.tgz";
      sha1 = "25f48ea22ca79187f3174a4db8759347bb126220";
    };
    deps = {
      "ansi-regex-0.2.1" = self.by-version."ansi-regex"."0.2.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."strip-ansi"."^3.0.0" =
    self.by-version."strip-ansi"."3.0.1";
  by-version."strip-ansi"."3.0.1" = self.buildNodePackage {
    name = "strip-ansi-3.0.1";
    version = "3.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/strip-ansi/-/strip-ansi-3.0.1.tgz";
      name = "strip-ansi-3.0.1.tgz";
      sha1 = "6a385fb8853d952d5ff05d0e8aaf94278dc63dcf";
    };
    deps = {
      "ansi-regex-2.0.0" = self.by-version."ansi-regex"."2.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."strip-ansi"."^3.0.1" =
    self.by-version."strip-ansi"."3.0.1";
  by-spec."strip-ansi"."~0.1.0" =
    self.by-version."strip-ansi"."0.1.1";
  by-version."strip-ansi"."0.1.1" = self.buildNodePackage {
    name = "strip-ansi-0.1.1";
    version = "0.1.1";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/strip-ansi/-/strip-ansi-0.1.1.tgz";
      name = "strip-ansi-0.1.1.tgz";
      sha1 = "39e8a98d044d150660abe4a6808acf70bb7bc991";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."strip-bom"."^1.0.0" =
    self.by-version."strip-bom"."1.0.0";
  by-version."strip-bom"."1.0.0" = self.buildNodePackage {
    name = "strip-bom-1.0.0";
    version = "1.0.0";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/strip-bom/-/strip-bom-1.0.0.tgz";
      name = "strip-bom-1.0.0.tgz";
      sha1 = "85b8862f3844b5a6d5ec8467a93598173a36f794";
    };
    deps = {
      "first-chunk-stream-1.0.0" = self.by-version."first-chunk-stream"."1.0.0";
      "is-utf8-0.2.1" = self.by-version."is-utf8"."0.2.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."strip-bom"."^2.0.0" =
    self.by-version."strip-bom"."2.0.0";
  by-version."strip-bom"."2.0.0" = self.buildNodePackage {
    name = "strip-bom-2.0.0";
    version = "2.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/strip-bom/-/strip-bom-2.0.0.tgz";
      name = "strip-bom-2.0.0.tgz";
      sha1 = "6219a85616520491f35788bdbf1447a99c7e6b0e";
    };
    deps = {
      "is-utf8-0.2.1" = self.by-version."is-utf8"."0.2.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."strip-bom-stream"."^1.0.0" =
    self.by-version."strip-bom-stream"."1.0.0";
  by-version."strip-bom-stream"."1.0.0" = self.buildNodePackage {
    name = "strip-bom-stream-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/strip-bom-stream/-/strip-bom-stream-1.0.0.tgz";
      name = "strip-bom-stream-1.0.0.tgz";
      sha1 = "e7144398577d51a6bed0fa1994fa05f43fd988ee";
    };
    deps = {
      "first-chunk-stream-1.0.0" = self.by-version."first-chunk-stream"."1.0.0";
      "strip-bom-2.0.0" = self.by-version."strip-bom"."2.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."strip-dirs"."^1.0.0" =
    self.by-version."strip-dirs"."1.1.1";
  by-version."strip-dirs"."1.1.1" = self.buildNodePackage {
    name = "strip-dirs-1.1.1";
    version = "1.1.1";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/strip-dirs/-/strip-dirs-1.1.1.tgz";
      name = "strip-dirs-1.1.1.tgz";
      sha1 = "960bbd1287844f3975a4558aa103a8255e2456a0";
    };
    deps = {
      "chalk-1.1.3" = self.by-version."chalk"."1.1.3";
      "get-stdin-4.0.1" = self.by-version."get-stdin"."4.0.1";
      "is-absolute-0.1.7" = self.by-version."is-absolute"."0.1.7";
      "is-natural-number-2.1.1" = self.by-version."is-natural-number"."2.1.1";
      "minimist-1.2.0" = self.by-version."minimist"."1.2.0";
      "sum-up-1.0.3" = self.by-version."sum-up"."1.0.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."strip-indent"."^1.0.0" =
    self.by-version."strip-indent"."1.0.1";
  by-version."strip-indent"."1.0.1" = self.buildNodePackage {
    name = "strip-indent-1.0.1";
    version = "1.0.1";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/strip-indent/-/strip-indent-1.0.1.tgz";
      name = "strip-indent-1.0.1.tgz";
      sha1 = "0c7962a6adefa7bbd4ac366460a638552ae1a0a2";
    };
    deps = {
      "get-stdin-4.0.1" = self.by-version."get-stdin"."4.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."strip-indent"."^1.0.1" =
    self.by-version."strip-indent"."1.0.1";
  by-spec."strip-json-comments"."~1.0.4" =
    self.by-version."strip-json-comments"."1.0.4";
  by-version."strip-json-comments"."1.0.4" = self.buildNodePackage {
    name = "strip-json-comments-1.0.4";
    version = "1.0.4";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/strip-json-comments/-/strip-json-comments-1.0.4.tgz";
      name = "strip-json-comments-1.0.4.tgz";
      sha1 = "1e15fbcac97d3ee99bf2d73b4c656b082bbafb91";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."strip-outer"."^1.0.0" =
    self.by-version."strip-outer"."1.0.0";
  by-version."strip-outer"."1.0.0" = self.buildNodePackage {
    name = "strip-outer-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/strip-outer/-/strip-outer-1.0.0.tgz";
      name = "strip-outer-1.0.0.tgz";
      sha1 = "aac0ba60d2e90c5d4f275fd8869fd9a2d310ffb8";
    };
    deps = {
      "escape-string-regexp-1.0.5" = self.by-version."escape-string-regexp"."1.0.5";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."style-data"."^1.1.0" =
    self.by-version."style-data"."1.1.2";
  by-version."style-data"."1.1.2" = self.buildNodePackage {
    name = "style-data-1.1.2";
    version = "1.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/style-data/-/style-data-1.1.2.tgz";
      name = "style-data-1.1.2.tgz";
      sha1 = "d9f4a802ecb7643316560d364c4b5afa3935a89d";
    };
    deps = {
      "cheerio-0.19.0" = self.by-version."cheerio"."0.19.0";
      "extend-3.0.0" = self.by-version."extend"."3.0.0";
      "mediaquery-text-1.0.2" = self.by-version."mediaquery-text"."1.0.2";
      "object.pick-1.1.2" = self.by-version."object.pick"."1.1.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."style-selector"."^2.0.0" =
    self.by-version."style-selector"."2.0.1";
  by-version."style-selector"."2.0.1" = self.buildNodePackage {
    name = "style-selector-2.0.1";
    version = "2.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/style-selector/-/style-selector-2.0.1.tgz";
      name = "style-selector-2.0.1.tgz";
      sha1 = "6c257f5b2140ae18435e9db15d424e4ef63e446c";
    };
    deps = {
      "slick-1.12.2" = self.by-version."slick"."1.12.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."sum-up"."^1.0.1" =
    self.by-version."sum-up"."1.0.3";
  by-version."sum-up"."1.0.3" = self.buildNodePackage {
    name = "sum-up-1.0.3";
    version = "1.0.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/sum-up/-/sum-up-1.0.3.tgz";
      name = "sum-up-1.0.3.tgz";
      sha1 = "1c661f667057f63bcb7875aa1438bc162525156e";
    };
    deps = {
      "chalk-1.1.3" = self.by-version."chalk"."1.1.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."superagent"."^1.1.0" =
    self.by-version."superagent"."1.8.4";
  by-version."superagent"."1.8.4" = self.buildNodePackage {
    name = "superagent-1.8.4";
    version = "1.8.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/superagent/-/superagent-1.8.4.tgz";
      name = "superagent-1.8.4.tgz";
      sha1 = "68b2c8a400f1753ddb39410906899e42f420fd3a";
    };
    deps = {
      "qs-2.3.3" = self.by-version."qs"."2.3.3";
      "formidable-1.0.17" = self.by-version."formidable"."1.0.17";
      "mime-1.3.4" = self.by-version."mime"."1.3.4";
      "component-emitter-1.2.1" = self.by-version."component-emitter"."1.2.1";
      "methods-1.1.2" = self.by-version."methods"."1.1.2";
      "cookiejar-2.0.6" = self.by-version."cookiejar"."2.0.6";
      "debug-2.2.0" = self.by-version."debug"."2.2.0";
      "reduce-component-1.0.1" = self.by-version."reduce-component"."1.0.1";
      "extend-3.0.0" = self.by-version."extend"."3.0.0";
      "form-data-1.0.0-rc3" = self.by-version."form-data"."1.0.0-rc3";
      "readable-stream-1.0.27-1" = self.by-version."readable-stream"."1.0.27-1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."supports-color"."^0.2.0" =
    self.by-version."supports-color"."0.2.0";
  by-version."supports-color"."0.2.0" = self.buildNodePackage {
    name = "supports-color-0.2.0";
    version = "0.2.0";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/supports-color/-/supports-color-0.2.0.tgz";
      name = "supports-color-0.2.0.tgz";
      sha1 = "d92de2694eb3f67323973d7ae3d8b55b4c22190a";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."supports-color"."^2.0.0" =
    self.by-version."supports-color"."2.0.0";
  by-version."supports-color"."2.0.0" = self.buildNodePackage {
    name = "supports-color-2.0.0";
    version = "2.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/supports-color/-/supports-color-2.0.0.tgz";
      name = "supports-color-2.0.0.tgz";
      sha1 = "535d045ce6b6363fa40117084629995e9df324c7";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."svgo"."^0.6.0" =
    self.by-version."svgo"."0.6.6";
  by-version."svgo"."0.6.6" = self.buildNodePackage {
    name = "svgo-0.6.6";
    version = "0.6.6";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/svgo/-/svgo-0.6.6.tgz";
      name = "svgo-0.6.6.tgz";
      sha1 = "b340889036f20f9b447543077d0f5573ed044c08";
    };
    deps = {
      "sax-1.2.1" = self.by-version."sax"."1.2.1";
      "coa-1.0.1" = self.by-version."coa"."1.0.1";
      "js-yaml-3.6.1" = self.by-version."js-yaml"."3.6.1";
      "colors-1.1.2" = self.by-version."colors"."1.1.2";
      "whet.extend-0.9.9" = self.by-version."whet.extend"."0.9.9";
      "mkdirp-0.5.1" = self.by-version."mkdirp"."0.5.1";
      "csso-2.0.0" = self.by-version."csso"."2.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."swap-case"."^1.1.0" =
    self.by-version."swap-case"."1.1.2";
  by-version."swap-case"."1.1.2" = self.buildNodePackage {
    name = "swap-case-1.1.2";
    version = "1.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/swap-case/-/swap-case-1.1.2.tgz";
      name = "swap-case-1.1.2.tgz";
      sha1 = "c39203a4587385fad3c850a0bd1bcafa081974e3";
    };
    deps = {
      "lower-case-1.1.3" = self.by-version."lower-case"."1.1.3";
      "upper-case-1.1.3" = self.by-version."upper-case"."1.1.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."symbol"."^0.2.1" =
    self.by-version."symbol"."0.2.3";
  by-version."symbol"."0.2.3" = self.buildNodePackage {
    name = "symbol-0.2.3";
    version = "0.2.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/symbol/-/symbol-0.2.3.tgz";
      name = "symbol-0.2.3.tgz";
      sha1 = "3b9873b8a901e47c6efe21526a3ac372ef28bbc7";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."symbol-tree".">= 3.1.0 < 4.0.0" =
    self.by-version."symbol-tree"."3.1.4";
  by-version."symbol-tree"."3.1.4" = self.buildNodePackage {
    name = "symbol-tree-3.1.4";
    version = "3.1.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/symbol-tree/-/symbol-tree-3.1.4.tgz";
      name = "symbol-tree-3.1.4.tgz";
      sha1 = "02b279348d337debc39694c5c95f882d448a312a";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."tar"."^2.0.0" =
    self.by-version."tar"."2.2.1";
  by-version."tar"."2.2.1" = self.buildNodePackage {
    name = "tar-2.2.1";
    version = "2.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/tar/-/tar-2.2.1.tgz";
      name = "tar-2.2.1.tgz";
      sha1 = "8e4d2a256c0e2185c6b18ad694aec968b83cb1d1";
    };
    deps = {
      "block-stream-0.0.9" = self.by-version."block-stream"."0.0.9";
      "fstream-1.0.10" = self.by-version."fstream"."1.0.10";
      "inherits-2.0.1" = self.by-version."inherits"."2.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."tar"."~2.2.0" =
    self.by-version."tar"."2.2.1";
  by-spec."tar"."~2.2.1" =
    self.by-version."tar"."2.2.1";
  by-spec."tar-pack"."~3.1.0" =
    self.by-version."tar-pack"."3.1.4";
  by-version."tar-pack"."3.1.4" = self.buildNodePackage {
    name = "tar-pack-3.1.4";
    version = "3.1.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/tar-pack/-/tar-pack-3.1.4.tgz";
      name = "tar-pack-3.1.4.tgz";
      sha1 = "bc8cf9a22f5832739f12f3910dac1eb97b49708c";
    };
    deps = {
      "debug-2.2.0" = self.by-version."debug"."2.2.0";
      "fstream-1.0.10" = self.by-version."fstream"."1.0.10";
      "fstream-ignore-1.0.5" = self.by-version."fstream-ignore"."1.0.5";
      "once-1.3.3" = self.by-version."once"."1.3.3";
      "readable-stream-2.1.5" = self.by-version."readable-stream"."2.1.5";
      "rimraf-2.5.4" = self.by-version."rimraf"."2.5.4";
      "tar-2.2.1" = self.by-version."tar"."2.2.1";
      "uid-number-0.0.6" = self.by-version."uid-number"."0.0.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."tar-stream"."^1.1.1" =
    self.by-version."tar-stream"."1.5.2";
  by-version."tar-stream"."1.5.2" = self.buildNodePackage {
    name = "tar-stream-1.5.2";
    version = "1.5.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/tar-stream/-/tar-stream-1.5.2.tgz";
      name = "tar-stream-1.5.2.tgz";
      sha1 = "fbc6c6e83c1a19d4cb48c7d96171fc248effc7bf";
    };
    deps = {
      "bl-1.1.2" = self.by-version."bl"."1.1.2";
      "end-of-stream-1.1.0" = self.by-version."end-of-stream"."1.1.0";
      "readable-stream-2.1.5" = self.by-version."readable-stream"."2.1.5";
      "xtend-4.0.1" = self.by-version."xtend"."4.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."tempfile"."^1.0.0" =
    self.by-version."tempfile"."1.1.1";
  by-version."tempfile"."1.1.1" = self.buildNodePackage {
    name = "tempfile-1.1.1";
    version = "1.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/tempfile/-/tempfile-1.1.1.tgz";
      name = "tempfile-1.1.1.tgz";
      sha1 = "5bcc4eaecc4ab2c707d8bc11d99ccc9a2cb287f2";
    };
    deps = {
      "os-tmpdir-1.0.1" = self.by-version."os-tmpdir"."1.0.1";
      "uuid-2.0.2" = self.by-version."uuid"."2.0.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."ternary-stream"."^2.0.0" =
    self.by-version."ternary-stream"."2.0.0";
  by-version."ternary-stream"."2.0.0" = self.buildNodePackage {
    name = "ternary-stream-2.0.0";
    version = "2.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/ternary-stream/-/ternary-stream-2.0.0.tgz";
      name = "ternary-stream-2.0.0.tgz";
      sha1 = "569fa9042b01456b7a1277c64f41d643d84570e6";
    };
    deps = {
      "duplexify-3.4.5" = self.by-version."duplexify"."3.4.5";
      "fork-stream-0.0.4" = self.by-version."fork-stream"."0.0.4";
      "merge-stream-1.0.0" = self.by-version."merge-stream"."1.0.0";
      "through2-2.0.1" = self.by-version."through2"."2.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."textextensions"."~1.0.0" =
    self.by-version."textextensions"."1.0.2";
  by-version."textextensions"."1.0.2" = self.buildNodePackage {
    name = "textextensions-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/textextensions/-/textextensions-1.0.2.tgz";
      name = "textextensions-1.0.2.tgz";
      sha1 = "65486393ee1f2bb039a60cbba05b0b68bd9501d2";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."tfunk"."^3.0.1" =
    self.by-version."tfunk"."3.0.2";
  by-version."tfunk"."3.0.2" = self.buildNodePackage {
    name = "tfunk-3.0.2";
    version = "3.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/tfunk/-/tfunk-3.0.2.tgz";
      name = "tfunk-3.0.2.tgz";
      sha1 = "327ebc6176af2680c6cd0d6d22297c79d7f96efd";
    };
    deps = {
      "chalk-1.1.3" = self.by-version."chalk"."1.1.3";
      "object-path-0.9.2" = self.by-version."object-path"."0.9.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."through"."2" =
    self.by-version."through"."2.3.8";
  by-version."through"."2.3.8" = self.buildNodePackage {
    name = "through-2.3.8";
    version = "2.3.8";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/through/-/through-2.3.8.tgz";
      name = "through-2.3.8.tgz";
      sha1 = "0dd4c9ffaabc357960b1b724115d7e0e86a2e1f5";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."through"."~2.3" =
    self.by-version."through"."2.3.8";
  by-spec."through"."~2.3.1" =
    self.by-version."through"."2.3.8";
  by-spec."through"."~2.3.4" =
    self.by-version."through"."2.3.8";
  by-spec."through2"."0.x" =
    self.by-version."through2"."0.6.5";
  by-version."through2"."0.6.5" = self.buildNodePackage {
    name = "through2-0.6.5";
    version = "0.6.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/through2/-/through2-0.6.5.tgz";
      name = "through2-0.6.5.tgz";
      sha1 = "41ab9c67b29d57209071410e1d7a7a968cd3ad48";
    };
    deps = {
      "readable-stream-1.0.34" = self.by-version."readable-stream"."1.0.34";
      "xtend-4.0.1" = self.by-version."xtend"."4.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."through2"."^0.5.0" =
    self.by-version."through2"."0.5.1";
  by-version."through2"."0.5.1" = self.buildNodePackage {
    name = "through2-0.5.1";
    version = "0.5.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/through2/-/through2-0.5.1.tgz";
      name = "through2-0.5.1.tgz";
      sha1 = "dfdd012eb9c700e2323fd334f38ac622ab372da7";
    };
    deps = {
      "readable-stream-1.0.34" = self.by-version."readable-stream"."1.0.34";
      "xtend-3.0.0" = self.by-version."xtend"."3.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."through2"."^0.6.0" =
    self.by-version."through2"."0.6.5";
  by-spec."through2"."^0.6.1" =
    self.by-version."through2"."0.6.5";
  by-spec."through2"."^2.0.0" =
    self.by-version."through2"."2.0.1";
  by-version."through2"."2.0.1" = self.buildNodePackage {
    name = "through2-2.0.1";
    version = "2.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/through2/-/through2-2.0.1.tgz";
      name = "through2-2.0.1.tgz";
      sha1 = "384e75314d49f32de12eebb8136b8eb6b5d59da9";
    };
    deps = {
      "readable-stream-2.0.6" = self.by-version."readable-stream"."2.0.6";
      "xtend-4.0.1" = self.by-version."xtend"."4.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."through2"."^2.0.1" =
    self.by-version."through2"."2.0.1";
  by-spec."through2"."~0.4.1" =
    self.by-version."through2"."0.4.2";
  by-version."through2"."0.4.2" = self.buildNodePackage {
    name = "through2-0.4.2";
    version = "0.4.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/through2/-/through2-0.4.2.tgz";
      name = "through2-0.4.2.tgz";
      sha1 = "dbf5866031151ec8352bb6c4db64a2292a840b9b";
    };
    deps = {
      "readable-stream-1.0.34" = self.by-version."readable-stream"."1.0.34";
      "xtend-2.1.2" = self.by-version."xtend"."2.1.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."through2"."~0.6.1" =
    self.by-version."through2"."0.6.5";
  by-spec."through2"."~2.0.0" =
    self.by-version."through2"."2.0.1";
  by-spec."through2-concurrent"."^1.1.0" =
    self.by-version."through2-concurrent"."1.1.1";
  by-version."through2-concurrent"."1.1.1" = self.buildNodePackage {
    name = "through2-concurrent-1.1.1";
    version = "1.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/through2-concurrent/-/through2-concurrent-1.1.1.tgz";
      name = "through2-concurrent-1.1.1.tgz";
      sha1 = "11cb4ea4c9e31bca6e4c1e6dba48d1c728c3524b";
    };
    deps = {
      "through2-2.0.1" = self.by-version."through2"."2.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."through2-filter"."^2.0.0" =
    self.by-version."through2-filter"."2.0.0";
  by-version."through2-filter"."2.0.0" = self.buildNodePackage {
    name = "through2-filter-2.0.0";
    version = "2.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/through2-filter/-/through2-filter-2.0.0.tgz";
      name = "through2-filter-2.0.0.tgz";
      sha1 = "60bc55a0dacb76085db1f9dae99ab43f83d622ec";
    };
    deps = {
      "through2-2.0.1" = self.by-version."through2"."2.0.1";
      "xtend-4.0.1" = self.by-version."xtend"."4.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."tildify"."^1.0.0" =
    self.by-version."tildify"."1.2.0";
  by-version."tildify"."1.2.0" = self.buildNodePackage {
    name = "tildify-1.2.0";
    version = "1.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/tildify/-/tildify-1.2.0.tgz";
      name = "tildify-1.2.0.tgz";
      sha1 = "dcec03f55dca9b7aa3e5b04f21817eb56e63588a";
    };
    deps = {
      "os-homedir-1.0.1" = self.by-version."os-homedir"."1.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."time-stamp"."^1.0.0" =
    self.by-version."time-stamp"."1.0.1";
  by-version."time-stamp"."1.0.1" = self.buildNodePackage {
    name = "time-stamp-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/time-stamp/-/time-stamp-1.0.1.tgz";
      name = "time-stamp-1.0.1.tgz";
      sha1 = "9f4bd23559c9365966f3302dbba2b07c6b99b151";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."timed-out"."^2.0.0" =
    self.by-version."timed-out"."2.0.0";
  by-version."timed-out"."2.0.0" = self.buildNodePackage {
    name = "timed-out-2.0.0";
    version = "2.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/timed-out/-/timed-out-2.0.0.tgz";
      name = "timed-out-2.0.0.tgz";
      sha1 = "f38b0ae81d3747d628001f41dafc652ace671c0a";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."title-case"."^1.1.0" =
    self.by-version."title-case"."1.1.2";
  by-version."title-case"."1.1.2" = self.buildNodePackage {
    name = "title-case-1.1.2";
    version = "1.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/title-case/-/title-case-1.1.2.tgz";
      name = "title-case-1.1.2.tgz";
      sha1 = "fae4a6ae546bfa22d083a0eea910a40d12ed4f5a";
    };
    deps = {
      "sentence-case-1.1.3" = self.by-version."sentence-case"."1.1.3";
      "upper-case-1.1.3" = self.by-version."upper-case"."1.1.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."to-absolute-glob"."^0.1.1" =
    self.by-version."to-absolute-glob"."0.1.1";
  by-version."to-absolute-glob"."0.1.1" = self.buildNodePackage {
    name = "to-absolute-glob-0.1.1";
    version = "0.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/to-absolute-glob/-/to-absolute-glob-0.1.1.tgz";
      name = "to-absolute-glob-0.1.1.tgz";
      sha1 = "1cdfa472a9ef50c239ee66999b662ca0eb39937f";
    };
    deps = {
      "extend-shallow-2.0.1" = self.by-version."extend-shallow"."2.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."to-array"."0.1.4" =
    self.by-version."to-array"."0.1.4";
  by-version."to-array"."0.1.4" = self.buildNodePackage {
    name = "to-array-0.1.4";
    version = "0.1.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/to-array/-/to-array-0.1.4.tgz";
      name = "to-array-0.1.4.tgz";
      sha1 = "17e6c11f73dd4f3d74cda7a4ff3238e9ad9bf890";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."to-fast-properties"."^1.0.1" =
    self.by-version."to-fast-properties"."1.0.2";
  by-version."to-fast-properties"."1.0.2" = self.buildNodePackage {
    name = "to-fast-properties-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/to-fast-properties/-/to-fast-properties-1.0.2.tgz";
      name = "to-fast-properties-1.0.2.tgz";
      sha1 = "f3f5c0c3ba7299a7ef99427e44633257ade43320";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."tough-cookie".">=0.12.0" =
    self.by-version."tough-cookie"."2.3.1";
  by-version."tough-cookie"."2.3.1" = self.buildNodePackage {
    name = "tough-cookie-2.3.1";
    version = "2.3.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/tough-cookie/-/tough-cookie-2.3.1.tgz";
      name = "tough-cookie-2.3.1.tgz";
      sha1 = "99c77dfbb7d804249e8a299d4cb0fd81fef083fd";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."tough-cookie"."^2.2.0" =
    self.by-version."tough-cookie"."2.3.1";
  by-spec."tough-cookie"."~2.2.0" =
    self.by-version."tough-cookie"."2.2.2";
  by-version."tough-cookie"."2.2.2" = self.buildNodePackage {
    name = "tough-cookie-2.2.2";
    version = "2.2.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/tough-cookie/-/tough-cookie-2.2.2.tgz";
      name = "tough-cookie-2.2.2.tgz";
      sha1 = "c83a1830f4e5ef0b93ef2a3488e724f8de016ac7";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."tough-cookie"."~2.3.0" =
    self.by-version."tough-cookie"."2.3.1";
  by-spec."tr46"."~0.0.1" =
    self.by-version."tr46"."0.0.3";
  by-version."tr46"."0.0.3" = self.buildNodePackage {
    name = "tr46-0.0.3";
    version = "0.0.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/tr46/-/tr46-0.0.3.tgz";
      name = "tr46-0.0.3.tgz";
      sha1 = "8184fd347dac9cdc185992f3a6622e14b9d9ab6a";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."trim-newlines"."^1.0.0" =
    self.by-version."trim-newlines"."1.0.0";
  by-version."trim-newlines"."1.0.0" = self.buildNodePackage {
    name = "trim-newlines-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/trim-newlines/-/trim-newlines-1.0.0.tgz";
      name = "trim-newlines-1.0.0.tgz";
      sha1 = "5887966bb582a4503a41eb524f7d35011815a613";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."trim-repeated"."^1.0.0" =
    self.by-version."trim-repeated"."1.0.0";
  by-version."trim-repeated"."1.0.0" = self.buildNodePackage {
    name = "trim-repeated-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/trim-repeated/-/trim-repeated-1.0.0.tgz";
      name = "trim-repeated-1.0.0.tgz";
      sha1 = "e3646a2ea4e891312bf7eace6cfb05380bc01c21";
    };
    deps = {
      "escape-string-regexp-1.0.5" = self.by-version."escape-string-regexp"."1.0.5";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."tryit"."^1.0.1" =
    self.by-version."tryit"."1.0.2";
  by-version."tryit"."1.0.2" = self.buildNodePackage {
    name = "tryit-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/tryit/-/tryit-1.0.2.tgz";
      name = "tryit-1.0.2.tgz";
      sha1 = "c196b0073e6b1c595d93c9c830855b7acc32a453";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."tunnel-agent"."^0.4.0" =
    self.by-version."tunnel-agent"."0.4.3";
  by-version."tunnel-agent"."0.4.3" = self.buildNodePackage {
    name = "tunnel-agent-0.4.3";
    version = "0.4.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/tunnel-agent/-/tunnel-agent-0.4.3.tgz";
      name = "tunnel-agent-0.4.3.tgz";
      sha1 = "6373db76909fe570e08d73583365ed828a74eeeb";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."tunnel-agent"."~0.4.0" =
    self.by-version."tunnel-agent"."0.4.3";
  by-spec."tunnel-agent"."~0.4.1" =
    self.by-version."tunnel-agent"."0.4.3";
  by-spec."tweetnacl"."~0.13.0" =
    self.by-version."tweetnacl"."0.13.3";
  by-version."tweetnacl"."0.13.3" = self.buildNodePackage {
    name = "tweetnacl-0.13.3";
    version = "0.13.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/tweetnacl/-/tweetnacl-0.13.3.tgz";
      name = "tweetnacl-0.13.3.tgz";
      sha1 = "d628b56f3bcc3d5ae74ba9d4c1a704def5ab4b56";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."type-check"."~0.3.2" =
    self.by-version."type-check"."0.3.2";
  by-version."type-check"."0.3.2" = self.buildNodePackage {
    name = "type-check-0.3.2";
    version = "0.3.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/type-check/-/type-check-0.3.2.tgz";
      name = "type-check-0.3.2.tgz";
      sha1 = "5884cab512cf1d355e3fb784f30804b2b520db72";
    };
    deps = {
      "prelude-ls-1.1.2" = self.by-version."prelude-ls"."1.1.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."typedarray"."~0.0.5" =
    self.by-version."typedarray"."0.0.6";
  by-version."typedarray"."0.0.6" = self.buildNodePackage {
    name = "typedarray-0.0.6";
    version = "0.0.6";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/typedarray/-/typedarray-0.0.6.tgz";
      name = "typedarray-0.0.6.tgz";
      sha1 = "867ac74e3864187b1d3d47d996a78ec5c8830777";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."ua-parser-js"."^0.7.9" =
    self.by-version."ua-parser-js"."0.7.10";
  by-version."ua-parser-js"."0.7.10" = self.buildNodePackage {
    name = "ua-parser-js-0.7.10";
    version = "0.7.10";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/ua-parser-js/-/ua-parser-js-0.7.10.tgz";
      name = "ua-parser-js-0.7.10.tgz";
      sha1 = "917559ddcce07cbc09ece7d80495e4c268f4ef9f";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."ucfirst"."^1.0.0" =
    self.by-version."ucfirst"."1.0.0";
  by-version."ucfirst"."1.0.0" = self.buildNodePackage {
    name = "ucfirst-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/ucfirst/-/ucfirst-1.0.0.tgz";
      name = "ucfirst-1.0.0.tgz";
      sha1 = "4e105b6448d05e264ecec435e0b919363c5f2f2f";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."uglify-js"."2.4.x" =
    self.by-version."uglify-js"."2.4.24";
  by-version."uglify-js"."2.4.24" = self.buildNodePackage {
    name = "uglify-js-2.4.24";
    version = "2.4.24";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/uglify-js/-/uglify-js-2.4.24.tgz";
      name = "uglify-js-2.4.24.tgz";
      sha1 = "fad5755c1e1577658bb06ff9ab6e548c95bebd6e";
    };
    deps = {
      "async-0.2.10" = self.by-version."async"."0.2.10";
      "source-map-0.1.34" = self.by-version."source-map"."0.1.34";
      "uglify-to-browserify-1.0.2" = self.by-version."uglify-to-browserify"."1.0.2";
      "yargs-3.5.4" = self.by-version."yargs"."3.5.4";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."uglify-js"."^2.6" =
    self.by-version."uglify-js"."2.7.3";
  by-version."uglify-js"."2.7.3" = self.buildNodePackage {
    name = "uglify-js-2.7.3";
    version = "2.7.3";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/uglify-js/-/uglify-js-2.7.3.tgz";
      name = "uglify-js-2.7.3.tgz";
      sha1 = "39b3a7329b89f5ec507e344c6e22568698ef4868";
    };
    deps = {
      "async-0.2.10" = self.by-version."async"."0.2.10";
      "source-map-0.5.6" = self.by-version."source-map"."0.5.6";
      "uglify-to-browserify-1.0.2" = self.by-version."uglify-to-browserify"."1.0.2";
      "yargs-3.10.0" = self.by-version."yargs"."3.10.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."uglify-to-browserify"."~1.0.0" =
    self.by-version."uglify-to-browserify"."1.0.2";
  by-version."uglify-to-browserify"."1.0.2" = self.buildNodePackage {
    name = "uglify-to-browserify-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/uglify-to-browserify/-/uglify-to-browserify-1.0.2.tgz";
      name = "uglify-to-browserify-1.0.2.tgz";
      sha1 = "6e0924d6bda6b5afe349e39a6d632850a0f882b7";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."uid-number"."~0.0.6" =
    self.by-version."uid-number"."0.0.6";
  by-version."uid-number"."0.0.6" = self.buildNodePackage {
    name = "uid-number-0.0.6";
    version = "0.0.6";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/uid-number/-/uid-number-0.0.6.tgz";
      name = "uid-number-0.0.6.tgz";
      sha1 = "0ea10e8035e8eb5b8e4449f06da1c730663baa81";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."ultron"."1.0.x" =
    self.by-version."ultron"."1.0.2";
  by-version."ultron"."1.0.2" = self.buildNodePackage {
    name = "ultron-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/ultron/-/ultron-1.0.2.tgz";
      name = "ultron-1.0.2.tgz";
      sha1 = "ace116ab557cd197386a4e88f4685378c8b2e4fa";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."unc-path-regex"."^0.1.0" =
    self.by-version."unc-path-regex"."0.1.2";
  by-version."unc-path-regex"."0.1.2" = self.buildNodePackage {
    name = "unc-path-regex-0.1.2";
    version = "0.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/unc-path-regex/-/unc-path-regex-0.1.2.tgz";
      name = "unc-path-regex-0.1.2.tgz";
      sha1 = "e73dd3d7b0d7c5ed86fbac6b0ae7d8c6a69d50fa";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."underscore"."*" =
    self.by-version."underscore"."1.8.3";
  by-version."underscore"."1.8.3" = self.buildNodePackage {
    name = "underscore-1.8.3";
    version = "1.8.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/underscore/-/underscore-1.8.3.tgz";
      name = "underscore-1.8.3.tgz";
      sha1 = "4f3fb53b106e6097fcf9cb4109f2a5e9bdfa5022";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."underscore"."1.7.x" =
    self.by-version."underscore"."1.7.0";
  by-version."underscore"."1.7.0" = self.buildNodePackage {
    name = "underscore-1.7.0";
    version = "1.7.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/underscore/-/underscore-1.7.0.tgz";
      name = "underscore-1.7.0.tgz";
      sha1 = "6bbaf0877500d36be34ecaa584e0db9fef035209";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."underscore"."~1.5" =
    self.by-version."underscore"."1.5.2";
  by-version."underscore"."1.5.2" = self.buildNodePackage {
    name = "underscore-1.5.2";
    version = "1.5.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/underscore/-/underscore-1.5.2.tgz";
      name = "underscore-1.5.2.tgz";
      sha1 = "1335c5e4f5e6d33bbb4b006ba8c86a00f556de08";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."unique-stream"."^1.0.0" =
    self.by-version."unique-stream"."1.0.0";
  by-version."unique-stream"."1.0.0" = self.buildNodePackage {
    name = "unique-stream-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/unique-stream/-/unique-stream-1.0.0.tgz";
      name = "unique-stream-1.0.0.tgz";
      sha1 = "d59a4a75427447d9aa6c91e70263f8d26a4b104b";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."unique-stream"."^2.0.2" =
    self.by-version."unique-stream"."2.2.1";
  by-version."unique-stream"."2.2.1" = self.buildNodePackage {
    name = "unique-stream-2.2.1";
    version = "2.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/unique-stream/-/unique-stream-2.2.1.tgz";
      name = "unique-stream-2.2.1.tgz";
      sha1 = "5aa003cfbe94c5ff866c4e7d668bb1c4dbadb369";
    };
    deps = {
      "json-stable-stringify-1.0.1" = self.by-version."json-stable-stringify"."1.0.1";
      "through2-filter-2.0.0" = self.by-version."through2-filter"."2.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."unpipe"."~1.0.0" =
    self.by-version."unpipe"."1.0.0";
  by-version."unpipe"."1.0.0" = self.buildNodePackage {
    name = "unpipe-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/unpipe/-/unpipe-1.0.0.tgz";
      name = "unpipe-1.0.0.tgz";
      sha1 = "b2bf4ee8514aae6165b4817829d21b2ef49904ec";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."unzip-response"."^1.0.0" =
    self.by-version."unzip-response"."1.0.0";
  by-version."unzip-response"."1.0.0" = self.buildNodePackage {
    name = "unzip-response-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/unzip-response/-/unzip-response-1.0.0.tgz";
      name = "unzip-response-1.0.0.tgz";
      sha1 = "bfda54eeec658f00c2df4d4494b9dca0ca00f3e4";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."upper-case"."^1.0.3" =
    self.by-version."upper-case"."1.1.3";
  by-version."upper-case"."1.1.3" = self.buildNodePackage {
    name = "upper-case-1.1.3";
    version = "1.1.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/upper-case/-/upper-case-1.1.3.tgz";
      name = "upper-case-1.1.3.tgz";
      sha1 = "f6b4501c2ec4cdd26ba78be7222961de77621598";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."upper-case"."^1.1.0" =
    self.by-version."upper-case"."1.1.3";
  by-spec."upper-case"."^1.1.1" =
    self.by-version."upper-case"."1.1.3";
  by-spec."upper-case-first"."^1.1.0" =
    self.by-version."upper-case-first"."1.1.2";
  by-version."upper-case-first"."1.1.2" = self.buildNodePackage {
    name = "upper-case-first-1.1.2";
    version = "1.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/upper-case-first/-/upper-case-first-1.1.2.tgz";
      name = "upper-case-first-1.1.2.tgz";
      sha1 = "5d79bedcff14419518fd2edb0a0507c9b6859115";
    };
    deps = {
      "upper-case-1.1.3" = self.by-version."upper-case"."1.1.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."urix"."^0.1.0" =
    self.by-version."urix"."0.1.0";
  by-version."urix"."0.1.0" = self.buildNodePackage {
    name = "urix-0.1.0";
    version = "0.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/urix/-/urix-0.1.0.tgz";
      name = "urix-0.1.0.tgz";
      sha1 = "da937f7a62e21fec1fd18d49b35c2935067a6c72";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."urix"."~0.1.0" =
    self.by-version."urix"."0.1.0";
  by-spec."url-parse-lax"."^1.0.0" =
    self.by-version."url-parse-lax"."1.0.0";
  by-version."url-parse-lax"."1.0.0" = self.buildNodePackage {
    name = "url-parse-lax-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/url-parse-lax/-/url-parse-lax-1.0.0.tgz";
      name = "url-parse-lax-1.0.0.tgz";
      sha1 = "7af8f303645e9bd79a272e7a14ac68bc0609da73";
    };
    deps = {
      "prepend-http-1.0.4" = self.by-version."prepend-http"."1.0.4";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."url-regex"."^3.0.0" =
    self.by-version."url-regex"."3.2.0";
  by-version."url-regex"."3.2.0" = self.buildNodePackage {
    name = "url-regex-3.2.0";
    version = "3.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/url-regex/-/url-regex-3.2.0.tgz";
      name = "url-regex-3.2.0.tgz";
      sha1 = "dbad1e0c9e29e105dd0b1f09f6862f7fdb482724";
    };
    deps = {
      "ip-regex-1.0.3" = self.by-version."ip-regex"."1.0.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."user-home"."^1.1.1" =
    self.by-version."user-home"."1.1.1";
  by-version."user-home"."1.1.1" = self.buildNodePackage {
    name = "user-home-1.1.1";
    version = "1.1.1";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/user-home/-/user-home-1.1.1.tgz";
      name = "user-home-1.1.1.tgz";
      sha1 = "2b5be23a32b63a7c9deb8d0f28d485724a3df190";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."utf8"."2.1.0" =
    self.by-version."utf8"."2.1.0";
  by-version."utf8"."2.1.0" = self.buildNodePackage {
    name = "utf8-2.1.0";
    version = "2.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/utf8/-/utf8-2.1.0.tgz";
      name = "utf8-2.1.0.tgz";
      sha1 = "0cfec5c8052d44a23e3aaa908104e8075f95dfd5";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."util-deprecate"."~1.0.1" =
    self.by-version."util-deprecate"."1.0.2";
  by-version."util-deprecate"."1.0.2" = self.buildNodePackage {
    name = "util-deprecate-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/util-deprecate/-/util-deprecate-1.0.2.tgz";
      name = "util-deprecate-1.0.2.tgz";
      sha1 = "450d4dc9fa70de732762fbd2d4a28981419a0ccf";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."utils-merge"."1.0.0" =
    self.by-version."utils-merge"."1.0.0";
  by-version."utils-merge"."1.0.0" = self.buildNodePackage {
    name = "utils-merge-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/utils-merge/-/utils-merge-1.0.0.tgz";
      name = "utils-merge-1.0.0.tgz";
      sha1 = "0294fb922bb9375153541c4f7096231f287c8af8";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."uuid"."^2.0.1" =
    self.by-version."uuid"."2.0.2";
  by-version."uuid"."2.0.2" = self.buildNodePackage {
    name = "uuid-2.0.2";
    version = "2.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/uuid/-/uuid-2.0.2.tgz";
      name = "uuid-2.0.2.tgz";
      sha1 = "48bd5698f0677e3c7901a1c46ef15b1643794726";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."v8flags"."^2.0.2" =
    self.by-version."v8flags"."2.0.11";
  by-version."v8flags"."2.0.11" = self.buildNodePackage {
    name = "v8flags-2.0.11";
    version = "2.0.11";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/v8flags/-/v8flags-2.0.11.tgz";
      name = "v8flags-2.0.11.tgz";
      sha1 = "bca8f30f0d6d60612cc2c00641e6962d42ae6881";
    };
    deps = {
      "user-home-1.1.1" = self.by-version."user-home"."1.1.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."v8flags"."^2.0.9" =
    self.by-version."v8flags"."2.0.11";
  by-spec."vali-date"."^1.0.0" =
    self.by-version."vali-date"."1.0.0";
  by-version."vali-date"."1.0.0" = self.buildNodePackage {
    name = "vali-date-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/vali-date/-/vali-date-1.0.0.tgz";
      name = "vali-date-1.0.0.tgz";
      sha1 = "1b904a59609fb328ef078138420934f6b86709a6";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."validate-npm-package-license"."^3.0.1" =
    self.by-version."validate-npm-package-license"."3.0.1";
  by-version."validate-npm-package-license"."3.0.1" = self.buildNodePackage {
    name = "validate-npm-package-license-3.0.1";
    version = "3.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/validate-npm-package-license/-/validate-npm-package-license-3.0.1.tgz";
      name = "validate-npm-package-license-3.0.1.tgz";
      sha1 = "2804babe712ad3379459acfbe24746ab2c303fbc";
    };
    deps = {
      "spdx-correct-1.0.2" = self.by-version."spdx-correct"."1.0.2";
      "spdx-expression-parse-1.0.2" = self.by-version."spdx-expression-parse"."1.0.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."verror"."1.3.6" =
    self.by-version."verror"."1.3.6";
  by-version."verror"."1.3.6" = self.buildNodePackage {
    name = "verror-1.3.6";
    version = "1.3.6";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/verror/-/verror-1.3.6.tgz";
      name = "verror-1.3.6.tgz";
      sha1 = "cff5df12946d297d2baaefaa2689e25be01c005c";
    };
    deps = {
      "extsprintf-1.0.2" = self.by-version."extsprintf"."1.0.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."vinyl"."^0.2.1" =
    self.by-version."vinyl"."0.2.3";
  by-version."vinyl"."0.2.3" = self.buildNodePackage {
    name = "vinyl-0.2.3";
    version = "0.2.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/vinyl/-/vinyl-0.2.3.tgz";
      name = "vinyl-0.2.3.tgz";
      sha1 = "bca938209582ec5a49ad538a00fa1f125e513252";
    };
    deps = {
      "clone-stats-0.0.1" = self.by-version."clone-stats"."0.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."vinyl"."^0.4.0" =
    self.by-version."vinyl"."0.4.6";
  by-version."vinyl"."0.4.6" = self.buildNodePackage {
    name = "vinyl-0.4.6";
    version = "0.4.6";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/vinyl/-/vinyl-0.4.6.tgz";
      name = "vinyl-0.4.6.tgz";
      sha1 = "2f356c87a550a255461f36bbeb2a5ba8bf784847";
    };
    deps = {
      "clone-0.2.0" = self.by-version."clone"."0.2.0";
      "clone-stats-0.0.1" = self.by-version."clone-stats"."0.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."vinyl"."^0.4.3" =
    self.by-version."vinyl"."0.4.6";
  by-spec."vinyl"."^0.4.6" =
    self.by-version."vinyl"."0.4.6";
  by-spec."vinyl"."^0.5.0" =
    self.by-version."vinyl"."0.5.3";
  by-version."vinyl"."0.5.3" = self.buildNodePackage {
    name = "vinyl-0.5.3";
    version = "0.5.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/vinyl/-/vinyl-0.5.3.tgz";
      name = "vinyl-0.5.3.tgz";
      sha1 = "b0455b38fc5e0cf30d4325132e461970c2091cde";
    };
    deps = {
      "clone-1.0.2" = self.by-version."clone"."1.0.2";
      "clone-stats-0.0.1" = self.by-version."clone-stats"."0.0.1";
      "replace-ext-0.0.1" = self.by-version."replace-ext"."0.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."vinyl"."^1.0.0" =
    self.by-version."vinyl"."1.2.0";
  by-version."vinyl"."1.2.0" = self.buildNodePackage {
    name = "vinyl-1.2.0";
    version = "1.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/vinyl/-/vinyl-1.2.0.tgz";
      name = "vinyl-1.2.0.tgz";
      sha1 = "5c88036cf565e5df05558bfc911f8656df218884";
    };
    deps = {
      "clone-1.0.2" = self.by-version."clone"."1.0.2";
      "clone-stats-0.0.1" = self.by-version."clone-stats"."0.0.1";
      "replace-ext-0.0.1" = self.by-version."replace-ext"."0.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."vinyl"."~0.2.3" =
    self.by-version."vinyl"."0.2.3";
  by-spec."vinyl-assign"."^1.0.1" =
    self.by-version."vinyl-assign"."1.2.1";
  by-version."vinyl-assign"."1.2.1" = self.buildNodePackage {
    name = "vinyl-assign-1.2.1";
    version = "1.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/vinyl-assign/-/vinyl-assign-1.2.1.tgz";
      name = "vinyl-assign-1.2.1.tgz";
      sha1 = "4d198891b5515911d771a8cd9c5480a46a074a45";
    };
    deps = {
      "object-assign-4.1.0" = self.by-version."object-assign"."4.1.0";
      "readable-stream-2.1.5" = self.by-version."readable-stream"."2.1.5";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."vinyl-fs"."^0.3.0" =
    self.by-version."vinyl-fs"."0.3.14";
  by-version."vinyl-fs"."0.3.14" = self.buildNodePackage {
    name = "vinyl-fs-0.3.14";
    version = "0.3.14";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/vinyl-fs/-/vinyl-fs-0.3.14.tgz";
      name = "vinyl-fs-0.3.14.tgz";
      sha1 = "9a6851ce1cac1c1cea5fe86c0931d620c2cfa9e6";
    };
    deps = {
      "defaults-1.0.3" = self.by-version."defaults"."1.0.3";
      "glob-stream-3.1.18" = self.by-version."glob-stream"."3.1.18";
      "glob-watcher-0.0.6" = self.by-version."glob-watcher"."0.0.6";
      "graceful-fs-3.0.9" = self.by-version."graceful-fs"."3.0.9";
      "mkdirp-0.5.1" = self.by-version."mkdirp"."0.5.1";
      "strip-bom-1.0.0" = self.by-version."strip-bom"."1.0.0";
      "through2-0.6.5" = self.by-version."through2"."0.6.5";
      "vinyl-0.4.6" = self.by-version."vinyl"."0.4.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."vinyl-fs"."^2.1.1" =
    self.by-version."vinyl-fs"."2.4.3";
  by-version."vinyl-fs"."2.4.3" = self.buildNodePackage {
    name = "vinyl-fs-2.4.3";
    version = "2.4.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/vinyl-fs/-/vinyl-fs-2.4.3.tgz";
      name = "vinyl-fs-2.4.3.tgz";
      sha1 = "3d97e562ebfdd4b66921dea70626b84bde9d2d07";
    };
    deps = {
      "duplexify-3.4.5" = self.by-version."duplexify"."3.4.5";
      "glob-stream-5.3.2" = self.by-version."glob-stream"."5.3.2";
      "graceful-fs-4.1.5" = self.by-version."graceful-fs"."4.1.5";
      "gulp-sourcemaps-1.6.0" = self.by-version."gulp-sourcemaps"."1.6.0";
      "is-valid-glob-0.3.0" = self.by-version."is-valid-glob"."0.3.0";
      "lazystream-1.0.0" = self.by-version."lazystream"."1.0.0";
      "lodash.isequal-4.4.0" = self.by-version."lodash.isequal"."4.4.0";
      "merge-stream-1.0.0" = self.by-version."merge-stream"."1.0.0";
      "mkdirp-0.5.1" = self.by-version."mkdirp"."0.5.1";
      "object-assign-4.1.0" = self.by-version."object-assign"."4.1.0";
      "readable-stream-2.1.5" = self.by-version."readable-stream"."2.1.5";
      "strip-bom-2.0.0" = self.by-version."strip-bom"."2.0.0";
      "strip-bom-stream-1.0.0" = self.by-version."strip-bom-stream"."1.0.0";
      "through2-2.0.1" = self.by-version."through2"."2.0.1";
      "through2-filter-2.0.0" = self.by-version."through2-filter"."2.0.0";
      "vali-date-1.0.0" = self.by-version."vali-date"."1.0.0";
      "vinyl-1.2.0" = self.by-version."vinyl"."1.2.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."vinyl-fs"."^2.2.0" =
    self.by-version."vinyl-fs"."2.4.3";
  by-spec."vinyl-fs"."^2.3.0" =
    self.by-version."vinyl-fs"."2.4.3";
  by-spec."vinyl-sourcemaps-apply"."^0.2.0" =
    self.by-version."vinyl-sourcemaps-apply"."0.2.1";
  by-version."vinyl-sourcemaps-apply"."0.2.1" = self.buildNodePackage {
    name = "vinyl-sourcemaps-apply-0.2.1";
    version = "0.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/vinyl-sourcemaps-apply/-/vinyl-sourcemaps-apply-0.2.1.tgz";
      name = "vinyl-sourcemaps-apply-0.2.1.tgz";
      sha1 = "ab6549d61d172c2b1b87be5c508d239c8ef87705";
    };
    deps = {
      "source-map-0.5.6" = self.by-version."source-map"."0.5.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."ware"."^1.2.0" =
    self.by-version."ware"."1.3.0";
  by-version."ware"."1.3.0" = self.buildNodePackage {
    name = "ware-1.3.0";
    version = "1.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/ware/-/ware-1.3.0.tgz";
      name = "ware-1.3.0.tgz";
      sha1 = "d1b14f39d2e2cb4ab8c4098f756fe4b164e473d4";
    };
    deps = {
      "wrap-fn-0.1.5" = self.by-version."wrap-fn"."0.1.5";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."webidl-conversions"."^2.0.0" =
    self.by-version."webidl-conversions"."2.0.1";
  by-version."webidl-conversions"."2.0.1" = self.buildNodePackage {
    name = "webidl-conversions-2.0.1";
    version = "2.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/webidl-conversions/-/webidl-conversions-2.0.1.tgz";
      name = "webidl-conversions-2.0.1.tgz";
      sha1 = "3bf8258f7d318c7443c36f2e169402a1a6703506";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."weinre"."^2.0.0-pre-I0Z7U9OV" =
    self.by-version."weinre"."2.0.0-pre-I0Z7U9OV";
  by-version."weinre"."2.0.0-pre-I0Z7U9OV" = self.buildNodePackage {
    name = "weinre-2.0.0-pre-I0Z7U9OV";
    version = "2.0.0-pre-I0Z7U9OV";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/weinre/-/weinre-2.0.0-pre-I0Z7U9OV.tgz";
      name = "weinre-2.0.0-pre-I0Z7U9OV.tgz";
      sha1 = "fef8aa223921f7b40bbbbd4c3ed4302f6fd0a813";
    };
    deps = {
      "express-2.5.11" = self.by-version."express"."2.5.11";
      "nopt-3.0.6" = self.by-version."nopt"."3.0.6";
      "underscore-1.7.0" = self.by-version."underscore"."1.7.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."whatwg-url-compat"."~0.6.5" =
    self.by-version."whatwg-url-compat"."0.6.5";
  by-version."whatwg-url-compat"."0.6.5" = self.buildNodePackage {
    name = "whatwg-url-compat-0.6.5";
    version = "0.6.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/whatwg-url-compat/-/whatwg-url-compat-0.6.5.tgz";
      name = "whatwg-url-compat-0.6.5.tgz";
      sha1 = "00898111af689bb097541cd5a45ca6c8798445bf";
    };
    deps = {
      "tr46-0.0.3" = self.by-version."tr46"."0.0.3";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."whet.extend"."~0.9.9" =
    self.by-version."whet.extend"."0.9.9";
  by-version."whet.extend"."0.9.9" = self.buildNodePackage {
    name = "whet.extend-0.9.9";
    version = "0.9.9";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/whet.extend/-/whet.extend-0.9.9.tgz";
      name = "whet.extend-0.9.9.tgz";
      sha1 = "f877d5bf648c97e5aa542fadc16d6a259b9c11a1";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."which"."1" =
    self.by-version."which"."1.2.10";
  by-version."which"."1.2.10" = self.buildNodePackage {
    name = "which-1.2.10";
    version = "1.2.10";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/which/-/which-1.2.10.tgz";
      name = "which-1.2.10.tgz";
      sha1 = "91cd9bd0751322411b659b40f054b21de957ab2d";
    };
    deps = {
      "isexe-1.1.2" = self.by-version."isexe"."1.1.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."which"."^1.2.10" =
    self.by-version."which"."1.2.10";
  by-spec."which"."^1.2.9" =
    self.by-version."which"."1.2.10";
  by-spec."which-module"."^1.0.0" =
    self.by-version."which-module"."1.0.0";
  by-version."which-module"."1.0.0" = self.buildNodePackage {
    name = "which-module-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/which-module/-/which-module-1.0.0.tgz";
      name = "which-module-1.0.0.tgz";
      sha1 = "bba63ca861948994ff307736089e3b96026c2a4f";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."wide-align"."^1.1.0" =
    self.by-version."wide-align"."1.1.0";
  by-version."wide-align"."1.1.0" = self.buildNodePackage {
    name = "wide-align-1.1.0";
    version = "1.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/wide-align/-/wide-align-1.1.0.tgz";
      name = "wide-align-1.1.0.tgz";
      sha1 = "40edde802a71fea1f070da3e62dcda2e7add96ad";
    };
    deps = {
      "string-width-1.0.2" = self.by-version."string-width"."1.0.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."window-size"."0.1.0" =
    self.by-version."window-size"."0.1.0";
  by-version."window-size"."0.1.0" = self.buildNodePackage {
    name = "window-size-0.1.0";
    version = "0.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/window-size/-/window-size-0.1.0.tgz";
      name = "window-size-0.1.0.tgz";
      sha1 = "5438cd2ea93b202efa3a19fe8887aee7c94f9c9d";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."window-size"."^0.1.2" =
    self.by-version."window-size"."0.1.4";
  by-version."window-size"."0.1.4" = self.buildNodePackage {
    name = "window-size-0.1.4";
    version = "0.1.4";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/window-size/-/window-size-0.1.4.tgz";
      name = "window-size-0.1.4.tgz";
      sha1 = "f8e1aa1ee5a53ec5bf151ffa09742a6ad7697876";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."window-size"."^0.1.4" =
    self.by-version."window-size"."0.1.4";
  by-spec."window-size"."^0.2.0" =
    self.by-version."window-size"."0.2.0";
  by-version."window-size"."0.2.0" = self.buildNodePackage {
    name = "window-size-0.2.0";
    version = "0.2.0";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/window-size/-/window-size-0.2.0.tgz";
      name = "window-size-0.2.0.tgz";
      sha1 = "b4315bb4214a3d7058ebeee892e13fa24d98b075";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."wordwrap"."0.0.2" =
    self.by-version."wordwrap"."0.0.2";
  by-version."wordwrap"."0.0.2" = self.buildNodePackage {
    name = "wordwrap-0.0.2";
    version = "0.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/wordwrap/-/wordwrap-0.0.2.tgz";
      name = "wordwrap-0.0.2.tgz";
      sha1 = "b79669bb42ecb409f83d583cad52ca17eaa1643f";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."wordwrap"."~0.0.2" =
    self.by-version."wordwrap"."0.0.3";
  by-version."wordwrap"."0.0.3" = self.buildNodePackage {
    name = "wordwrap-0.0.3";
    version = "0.0.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/wordwrap/-/wordwrap-0.0.3.tgz";
      name = "wordwrap-0.0.3.tgz";
      sha1 = "a3d5da6cd5c0bc0008d37234bbaf1bed63059107";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."wordwrap"."~1.0.0" =
    self.by-version."wordwrap"."1.0.0";
  by-version."wordwrap"."1.0.0" = self.buildNodePackage {
    name = "wordwrap-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/wordwrap/-/wordwrap-1.0.0.tgz";
      name = "wordwrap-1.0.0.tgz";
      sha1 = "27584810891456a4171c8d0226441ade90cbcaeb";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."wrap-ansi"."^2.0.0" =
    self.by-version."wrap-ansi"."2.0.0";
  by-version."wrap-ansi"."2.0.0" = self.buildNodePackage {
    name = "wrap-ansi-2.0.0";
    version = "2.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/wrap-ansi/-/wrap-ansi-2.0.0.tgz";
      name = "wrap-ansi-2.0.0.tgz";
      sha1 = "7d30f8f873f9a5bbc3a64dabc8d177e071ae426f";
    };
    deps = {
      "string-width-1.0.2" = self.by-version."string-width"."1.0.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."wrap-fn"."^0.1.0" =
    self.by-version."wrap-fn"."0.1.5";
  by-version."wrap-fn"."0.1.5" = self.buildNodePackage {
    name = "wrap-fn-0.1.5";
    version = "0.1.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/wrap-fn/-/wrap-fn-0.1.5.tgz";
      name = "wrap-fn-0.1.5.tgz";
      sha1 = "f21b6e41016ff4a7e31720dbc63a09016bdf9845";
    };
    deps = {
      "co-3.1.0" = self.by-version."co"."3.1.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."wrappy"."1" =
    self.by-version."wrappy"."1.0.2";
  by-version."wrappy"."1.0.2" = self.buildNodePackage {
    name = "wrappy-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/wrappy/-/wrappy-1.0.2.tgz";
      name = "wrappy-1.0.2.tgz";
      sha1 = "b5243d8f3ec1aa35f1364605bc0d1036e30ab69f";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."wreck"."^6.3.0" =
    self.by-version."wreck"."6.3.0";
  by-version."wreck"."6.3.0" = self.buildNodePackage {
    name = "wreck-6.3.0";
    version = "6.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/wreck/-/wreck-6.3.0.tgz";
      name = "wreck-6.3.0.tgz";
      sha1 = "a1369769f07bbb62d6a378336a7871fc773c740b";
    };
    deps = {
      "hoek-2.16.3" = self.by-version."hoek"."2.16.3";
      "boom-2.10.1" = self.by-version."boom"."2.10.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."ws"."1.0.1" =
    self.by-version."ws"."1.0.1";
  by-version."ws"."1.0.1" = self.buildNodePackage {
    name = "ws-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/ws/-/ws-1.0.1.tgz";
      name = "ws-1.0.1.tgz";
      sha1 = "7d0b2a2e58cddd819039c29c9de65045e1b310e9";
    };
    deps = {
      "options-0.0.6" = self.by-version."options"."0.0.6";
      "ultron-1.0.2" = self.by-version."ultron"."1.0.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."xml-json"."^2.0.2" =
    self.by-version."xml-json"."2.0.2";
  by-version."xml-json"."2.0.2" = self.buildNodePackage {
    name = "xml-json-2.0.2";
    version = "2.0.2";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/xml-json/-/xml-json-2.0.2.tgz";
      name = "xml-json-2.0.2.tgz";
      sha1 = "0bc7e47eb72e8431fbd33dd7a85142090f3f2c17";
    };
    deps = {
      "extend-1.3.0" = self.by-version."extend"."1.3.0";
      "ldjson-stream-1.2.1" = self.by-version."ldjson-stream"."1.2.1";
      "minimist-0.2.0" = self.by-version."minimist"."0.2.0";
      "pumpify-1.3.5" = self.by-version."pumpify"."1.3.5";
      "xml-nodes-0.1.5" = self.by-version."xml-nodes"."0.1.5";
      "xml-objects-0.0.1" = self.by-version."xml-objects"."0.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."xml-name-validator".">= 2.0.1 < 3.0.0" =
    self.by-version."xml-name-validator"."2.0.1";
  by-version."xml-name-validator"."2.0.1" = self.buildNodePackage {
    name = "xml-name-validator-2.0.1";
    version = "2.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/xml-name-validator/-/xml-name-validator-2.0.1.tgz";
      name = "xml-name-validator-2.0.1.tgz";
      sha1 = "4d8b8f1eccd3419aa362061becef515e1e559635";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."xml-nodes"."^0.1.2" =
    self.by-version."xml-nodes"."0.1.5";
  by-version."xml-nodes"."0.1.5" = self.buildNodePackage {
    name = "xml-nodes-0.1.5";
    version = "0.1.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/xml-nodes/-/xml-nodes-0.1.5.tgz";
      name = "xml-nodes-0.1.5.tgz";
      sha1 = "9505c74dfd954867212c7d6f16d8c9fecafbb118";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."xml-objects"."0.0.1" =
    self.by-version."xml-objects"."0.0.1";
  by-version."xml-objects"."0.0.1" = self.buildNodePackage {
    name = "xml-objects-0.0.1";
    version = "0.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/xml-objects/-/xml-objects-0.0.1.tgz";
      name = "xml-objects-0.0.1.tgz";
      sha1 = "027da019cbbcc0babd503c8237e911cbac746a61";
    };
    deps = {
      "xml2js-0.4.17" = self.by-version."xml2js"."0.4.17";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."xml2js"."0.2.6" =
    self.by-version."xml2js"."0.2.6";
  by-version."xml2js"."0.2.6" = self.buildNodePackage {
    name = "xml2js-0.2.6";
    version = "0.2.6";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/xml2js/-/xml2js-0.2.6.tgz";
      name = "xml2js-0.2.6.tgz";
      sha1 = "d209c4e4dda1fc9c452141ef41c077f5adfdf6c4";
    };
    deps = {
      "sax-0.4.2" = self.by-version."sax"."0.4.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."xml2js"."0.4.15" =
    self.by-version."xml2js"."0.4.15";
  by-version."xml2js"."0.4.15" = self.buildNodePackage {
    name = "xml2js-0.4.15";
    version = "0.4.15";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/xml2js/-/xml2js-0.4.15.tgz";
      name = "xml2js-0.4.15.tgz";
      sha1 = "95cd03ff2dd144ec28bc6273bf2b2890c581ad0c";
    };
    deps = {
      "sax-1.2.1" = self.by-version."sax"."1.2.1";
      "xmlbuilder-8.2.2" = self.by-version."xmlbuilder"."8.2.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."xml2js"."~0.4.0" =
    self.by-version."xml2js"."0.4.17";
  by-version."xml2js"."0.4.17" = self.buildNodePackage {
    name = "xml2js-0.4.17";
    version = "0.4.17";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/xml2js/-/xml2js-0.4.17.tgz";
      name = "xml2js-0.4.17.tgz";
      sha1 = "17be93eaae3f3b779359c795b419705a8817e868";
    };
    deps = {
      "sax-1.2.1" = self.by-version."sax"."1.2.1";
      "xmlbuilder-4.2.1" = self.by-version."xmlbuilder"."4.2.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."xmlbuilder"."0.4.2" =
    self.by-version."xmlbuilder"."0.4.2";
  by-version."xmlbuilder"."0.4.2" = self.buildNodePackage {
    name = "xmlbuilder-0.4.2";
    version = "0.4.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/xmlbuilder/-/xmlbuilder-0.4.2.tgz";
      name = "xmlbuilder-0.4.2.tgz";
      sha1 = "1776d65f3fdbad470a08d8604cdeb1c4e540ff83";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."xmlbuilder"."2.6.2" =
    self.by-version."xmlbuilder"."2.6.2";
  by-version."xmlbuilder"."2.6.2" = self.buildNodePackage {
    name = "xmlbuilder-2.6.2";
    version = "2.6.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/xmlbuilder/-/xmlbuilder-2.6.2.tgz";
      name = "xmlbuilder-2.6.2.tgz";
      sha1 = "f916f6d10d45dc171b1be2e6e673fb6e0cc35d0a";
    };
    deps = {
      "lodash-3.5.0" = self.by-version."lodash"."3.5.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."xmlbuilder".">=2.4.6" =
    self.by-version."xmlbuilder"."8.2.2";
  by-version."xmlbuilder"."8.2.2" = self.buildNodePackage {
    name = "xmlbuilder-8.2.2";
    version = "8.2.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/xmlbuilder/-/xmlbuilder-8.2.2.tgz";
      name = "xmlbuilder-8.2.2.tgz";
      sha1 = "69248673410b4ba42e1a6136551d2922335aa773";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."xmlbuilder"."^4.1.0" =
    self.by-version."xmlbuilder"."4.2.1";
  by-version."xmlbuilder"."4.2.1" = self.buildNodePackage {
    name = "xmlbuilder-4.2.1";
    version = "4.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/xmlbuilder/-/xmlbuilder-4.2.1.tgz";
      name = "xmlbuilder-4.2.1.tgz";
      sha1 = "aa58a3041a066f90eaa16c2f5389ff19f3f461a5";
    };
    deps = {
      "lodash-4.15.0" = self.by-version."lodash"."4.15.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."xmlbuilder"."~2.2.1" =
    self.by-version."xmlbuilder"."2.2.1";
  by-version."xmlbuilder"."2.2.1" = self.buildNodePackage {
    name = "xmlbuilder-2.2.1";
    version = "2.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/xmlbuilder/-/xmlbuilder-2.2.1.tgz";
      name = "xmlbuilder-2.2.1.tgz";
      sha1 = "9326430f130d87435d4c4086643aa2926e105a32";
    };
    deps = {
      "lodash-node-2.4.1" = self.by-version."lodash-node"."2.4.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."xmlhttprequest-ssl"."1.5.1" =
    self.by-version."xmlhttprequest-ssl"."1.5.1";
  by-version."xmlhttprequest-ssl"."1.5.1" = self.buildNodePackage {
    name = "xmlhttprequest-ssl-1.5.1";
    version = "1.5.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/xmlhttprequest-ssl/-/xmlhttprequest-ssl-1.5.1.tgz";
      name = "xmlhttprequest-ssl-1.5.1.tgz";
      sha1 = "3b7741fea4a86675976e908d296d4445961faa67";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."xoauth2"."~0.1.8" =
    self.by-version."xoauth2"."0.1.8";
  by-version."xoauth2"."0.1.8" = self.buildNodePackage {
    name = "xoauth2-0.1.8";
    version = "0.1.8";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/xoauth2/-/xoauth2-0.1.8.tgz";
      name = "xoauth2-0.1.8.tgz";
      sha1 = "b916ff10ecfb54320f16f24a3e975120653ab0d2";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."xtend".">=4.0.0 <4.1.0-0" =
    self.by-version."xtend"."4.0.1";
  by-version."xtend"."4.0.1" = self.buildNodePackage {
    name = "xtend-4.0.1";
    version = "4.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/xtend/-/xtend-4.0.1.tgz";
      name = "xtend-4.0.1.tgz";
      sha1 = "a5c6d532be656e23db820efb943a1f04998d63af";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."xtend"."^4.0.0" =
    self.by-version."xtend"."4.0.1";
  by-spec."xtend"."~2.1.1" =
    self.by-version."xtend"."2.1.2";
  by-version."xtend"."2.1.2" = self.buildNodePackage {
    name = "xtend-2.1.2";
    version = "2.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/xtend/-/xtend-2.1.2.tgz";
      name = "xtend-2.1.2.tgz";
      sha1 = "6efecc2a4dad8e6962c4901b337ce7ba87b5d28b";
    };
    deps = {
      "object-keys-0.4.0" = self.by-version."object-keys"."0.4.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."xtend"."~3.0.0" =
    self.by-version."xtend"."3.0.0";
  by-version."xtend"."3.0.0" = self.buildNodePackage {
    name = "xtend-3.0.0";
    version = "3.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/xtend/-/xtend-3.0.0.tgz";
      name = "xtend-3.0.0.tgz";
      sha1 = "5cce7407baf642cba7becda568111c493f59665a";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."xtend"."~4.0.0" =
    self.by-version."xtend"."4.0.1";
  by-spec."y18n"."^3.2.0" =
    self.by-version."y18n"."3.2.1";
  by-version."y18n"."3.2.1" = self.buildNodePackage {
    name = "y18n-3.2.1";
    version = "3.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/y18n/-/y18n-3.2.1.tgz";
      name = "y18n-3.2.1.tgz";
      sha1 = "6d15fba884c08679c0d77e88e7759e811e07fa41";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."y18n"."^3.2.1" =
    self.by-version."y18n"."3.2.1";
  by-spec."yallist"."^2.0.0" =
    self.by-version."yallist"."2.0.0";
  by-version."yallist"."2.0.0" = self.buildNodePackage {
    name = "yallist-2.0.0";
    version = "2.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/yallist/-/yallist-2.0.0.tgz";
      name = "yallist-2.0.0.tgz";
      sha1 = "306c543835f09ee1a4cb23b7bce9ab341c91cdd4";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."yargs"."3.29.0" =
    self.by-version."yargs"."3.29.0";
  by-version."yargs"."3.29.0" = self.buildNodePackage {
    name = "yargs-3.29.0";
    version = "3.29.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/yargs/-/yargs-3.29.0.tgz";
      name = "yargs-3.29.0.tgz";
      sha1 = "1aab9660eae79d8b8f675bcaeeab6ee34c2cf69c";
    };
    deps = {
      "camelcase-1.2.1" = self.by-version."camelcase"."1.2.1";
      "cliui-3.2.0" = self.by-version."cliui"."3.2.0";
      "decamelize-1.2.0" = self.by-version."decamelize"."1.2.0";
      "os-locale-1.4.0" = self.by-version."os-locale"."1.4.0";
      "window-size-0.1.4" = self.by-version."window-size"."0.1.4";
      "y18n-3.2.1" = self.by-version."y18n"."3.2.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."yargs"."^3.28.0" =
    self.by-version."yargs"."3.32.0";
  by-version."yargs"."3.32.0" = self.buildNodePackage {
    name = "yargs-3.32.0";
    version = "3.32.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/yargs/-/yargs-3.32.0.tgz";
      name = "yargs-3.32.0.tgz";
      sha1 = "03088e9ebf9e756b69751611d2a5ef591482c995";
    };
    deps = {
      "camelcase-2.1.1" = self.by-version."camelcase"."2.1.1";
      "cliui-3.2.0" = self.by-version."cliui"."3.2.0";
      "decamelize-1.2.0" = self.by-version."decamelize"."1.2.0";
      "os-locale-1.4.0" = self.by-version."os-locale"."1.4.0";
      "string-width-1.0.2" = self.by-version."string-width"."1.0.2";
      "window-size-0.1.4" = self.by-version."window-size"."0.1.4";
      "y18n-3.2.1" = self.by-version."y18n"."3.2.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."yargs"."^4.7.1" =
    self.by-version."yargs"."4.8.1";
  by-version."yargs"."4.8.1" = self.buildNodePackage {
    name = "yargs-4.8.1";
    version = "4.8.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/yargs/-/yargs-4.8.1.tgz";
      name = "yargs-4.8.1.tgz";
      sha1 = "c0c42924ca4aaa6b0e6da1739dfb216439f9ddc0";
    };
    deps = {
      "cliui-3.2.0" = self.by-version."cliui"."3.2.0";
      "decamelize-1.2.0" = self.by-version."decamelize"."1.2.0";
      "get-caller-file-1.0.2" = self.by-version."get-caller-file"."1.0.2";
      "lodash.assign-4.2.0" = self.by-version."lodash.assign"."4.2.0";
      "os-locale-1.4.0" = self.by-version."os-locale"."1.4.0";
      "read-pkg-up-1.0.1" = self.by-version."read-pkg-up"."1.0.1";
      "require-directory-2.1.1" = self.by-version."require-directory"."2.1.1";
      "require-main-filename-1.0.1" = self.by-version."require-main-filename"."1.0.1";
      "set-blocking-2.0.0" = self.by-version."set-blocking"."2.0.0";
      "string-width-1.0.2" = self.by-version."string-width"."1.0.2";
      "which-module-1.0.0" = self.by-version."which-module"."1.0.0";
      "window-size-0.2.0" = self.by-version."window-size"."0.2.0";
      "y18n-3.2.1" = self.by-version."y18n"."3.2.1";
      "yargs-parser-2.4.1" = self.by-version."yargs-parser"."2.4.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."yargs"."~3.10.0" =
    self.by-version."yargs"."3.10.0";
  by-version."yargs"."3.10.0" = self.buildNodePackage {
    name = "yargs-3.10.0";
    version = "3.10.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/yargs/-/yargs-3.10.0.tgz";
      name = "yargs-3.10.0.tgz";
      sha1 = "f7ee7bd857dd7c1d2d38c0e74efbd681d1431fd1";
    };
    deps = {
      "camelcase-1.2.1" = self.by-version."camelcase"."1.2.1";
      "cliui-2.1.0" = self.by-version."cliui"."2.1.0";
      "decamelize-1.2.0" = self.by-version."decamelize"."1.2.0";
      "window-size-0.1.0" = self.by-version."window-size"."0.1.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."yargs"."~3.5.4" =
    self.by-version."yargs"."3.5.4";
  by-version."yargs"."3.5.4" = self.buildNodePackage {
    name = "yargs-3.5.4";
    version = "3.5.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/yargs/-/yargs-3.5.4.tgz";
      name = "yargs-3.5.4.tgz";
      sha1 = "d8aff8f665e94c34bd259bdebd1bfaf0ddd35361";
    };
    deps = {
      "camelcase-1.2.1" = self.by-version."camelcase"."1.2.1";
      "decamelize-1.2.0" = self.by-version."decamelize"."1.2.0";
      "window-size-0.1.0" = self.by-version."window-size"."0.1.0";
      "wordwrap-0.0.2" = self.by-version."wordwrap"."0.0.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."yargs"."~4.1.0" =
    self.by-version."yargs"."4.1.0";
  by-version."yargs"."4.1.0" = self.buildNodePackage {
    name = "yargs-4.1.0";
    version = "4.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/yargs/-/yargs-4.1.0.tgz";
      name = "yargs-4.1.0.tgz";
      sha1 = "035e5ea466ac7fea584b00353e33eae4082b9894";
    };
    deps = {
      "camelcase-2.1.1" = self.by-version."camelcase"."2.1.1";
      "cliui-3.2.0" = self.by-version."cliui"."3.2.0";
      "decamelize-1.2.0" = self.by-version."decamelize"."1.2.0";
      "os-locale-1.4.0" = self.by-version."os-locale"."1.4.0";
      "pkg-conf-1.1.3" = self.by-version."pkg-conf"."1.1.3";
      "read-pkg-up-1.0.1" = self.by-version."read-pkg-up"."1.0.1";
      "require-main-filename-1.0.1" = self.by-version."require-main-filename"."1.0.1";
      "string-width-1.0.2" = self.by-version."string-width"."1.0.2";
      "window-size-0.2.0" = self.by-version."window-size"."0.2.0";
      "y18n-3.2.1" = self.by-version."y18n"."3.2.1";
      "yargs-parser-2.4.1" = self.by-version."yargs-parser"."2.4.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "yargs" = self.by-version."yargs"."4.1.0";
  by-spec."yargs-parser"."^2.1.0" =
    self.by-version."yargs-parser"."2.4.1";
  by-version."yargs-parser"."2.4.1" = self.buildNodePackage {
    name = "yargs-parser-2.4.1";
    version = "2.4.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/yargs-parser/-/yargs-parser-2.4.1.tgz";
      name = "yargs-parser-2.4.1.tgz";
      sha1 = "85568de3cf150ff49fa51825f03a8c880ddcc5c4";
    };
    deps = {
      "camelcase-3.0.0" = self.by-version."camelcase"."3.0.0";
      "lodash.assign-4.2.0" = self.by-version."lodash.assign"."4.2.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."yargs-parser"."^2.4.1" =
    self.by-version."yargs-parser"."2.4.1";
  by-spec."yauzl"."^2.2.1" =
    self.by-version."yauzl"."2.6.0";
  by-version."yauzl"."2.6.0" = self.buildNodePackage {
    name = "yauzl-2.6.0";
    version = "2.6.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/yauzl/-/yauzl-2.6.0.tgz";
      name = "yauzl-2.6.0.tgz";
      sha1 = "40894d4587bb125500d05df45471cd5e114a76f9";
    };
    deps = {
      "fd-slicer-1.0.1" = self.by-version."fd-slicer"."1.0.1";
      "buffer-crc32-0.2.5" = self.by-version."buffer-crc32"."0.2.5";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."yazl"."^2.1.0" =
    self.by-version."yazl"."2.4.1";
  by-version."yazl"."2.4.1" = self.buildNodePackage {
    name = "yazl-2.4.1";
    version = "2.4.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/yazl/-/yazl-2.4.1.tgz";
      name = "yazl-2.4.1.tgz";
      sha1 = "2bc98ebdfeccf0c2b47cc36f82214bcb6d54484c";
    };
    deps = {
      "buffer-crc32-0.2.5" = self.by-version."buffer-crc32"."0.2.5";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."yeast"."0.1.2" =
    self.by-version."yeast"."0.1.2";
  by-version."yeast"."0.1.2" = self.buildNodePackage {
    name = "yeast-0.1.2";
    version = "0.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/yeast/-/yeast-0.1.2.tgz";
      name = "yeast-0.1.2.tgz";
      sha1 = "008e06d8094320c372dbc2f8ed76a0ca6c8ac419";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
}
