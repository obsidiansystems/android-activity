{ mkDerivation, aeson, async, attoparsec, base, base64-bytestring
, bytestring, constraints, containers, data-default, dependent-map
, dependent-sum, haskell-src-exts, lens, loch-th, mtl
, network-uri, old-locale, reflex, semigroups, stdenv
, template-haskell, text, time, timezone-series, transformers
, vector
}:
mkDerivation {
  pname = "focus-core";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson async attoparsec base base64-bytestring bytestring
    constraints containers data-default dependent-map dependent-sum
    haskell-src-exts lens loch-th mtl network-uri old-locale
    reflex semigroups template-haskell text time timezone-series
    transformers vector
  ];
  description = "Project Synopsis Here";
  license = stdenv.lib.licenses.unfree;
}
