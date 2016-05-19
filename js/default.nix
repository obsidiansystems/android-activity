{mkDerivation, focus-core, focus-emojione, focus-http-th, reflex, loch-th, reflex-dom, aeson, attoparsec, text, time, vector, ghcjs-dom, constraints, timezone-series, timezone-olson, raw-strings-qq, http-types, mmorph}:

mkDerivation {
  license = null;
  pname = "focus-js";
  version = "0.1";
  src = ./.;
  buildDepends = [ focus-core focus-emojione focus-http-th loch-th reflex reflex-dom aeson attoparsec text time vector ghcjs-dom constraints timezone-series timezone-olson raw-strings-qq http-types mmorph ];
}
