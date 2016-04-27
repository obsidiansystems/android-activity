{ mkDerivation, aeson, async, attoparsec, base, base64-bytestring
, bytestring, constraints, containers, data-default, dependent-map
, dependent-sum, haskell-src-exts, HList, lens, loch-th, mtl
, network-uri, old-locale, reflex, semigroups, stdenv, stripe
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
    haskell-src-exts HList lens loch-th mtl network-uri old-locale
    reflex semigroups stripe template-haskell text time timezone-series
    transformers vector
  ];
  description = "Project Synopsis Here";
  license = stdenv.lib.licenses.unfree;
}
