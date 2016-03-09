{mkDerivation, aeson, attoparsec, base64-bytestring, stripe, text, time, vector, network-uri, timezone-series, constraints, dependent-map, reflex, HList, file-embed, data-default, loch-th, lens}: 

mkDerivation {
  pname = "focus-core";
  license = null;
  version = "0.1";
  src = ./.;
  buildDepends = [
    aeson
    attoparsec
    base64-bytestring
    stripe
    text
    time
    vector
    network-uri
    timezone-series
    constraints
    dependent-map
    reflex
    HList
    file-embed
    data-default
    loch-th
    lens
  ];
}
