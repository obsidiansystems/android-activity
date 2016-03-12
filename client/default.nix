{ mkDerivation, aeson, base, bytestring, containers, focus-core
, http-types, lens, loch-th, mtl, network, stdenv, text, time
, transformers, websockets
}:
mkDerivation {
  pname = "focus-client";
  version = "0.1";
  src = ./.;
  buildDepends = [
    aeson base bytestring containers focus-core http-types lens loch-th mtl 
    network text time transformers websockets
  ];
  license = null;
}
