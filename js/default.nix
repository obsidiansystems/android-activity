{mkDerivation, focus-core, reflex, loch-th, reflex-dom, aeson, attoparsec, text, time, vector, ghcjs-dom, constraints, timezone-series, timezone-olson, raw-strings-qq}: 

mkDerivation {
  license = null;
  pname = "focus-js";
  version = "0.1";
  src = ./.;
  buildDepends = [ focus-core loch-th reflex reflex-dom aeson attoparsec text time vector ghcjs-dom constraints timezone-series timezone-olson raw-strings-qq ];
}
