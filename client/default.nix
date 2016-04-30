{ mkDerivation, aeson, async, base, bytestring, containers
, exceptions, focus-core, http-types, lens, loch-th, mtl, network
, semigroups, stdenv, stm, text, time, transformers, websockets
}:
mkDerivation {
  pname = "focus-client";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base bytestring containers exceptions focus-core
    http-types lens loch-th mtl network semigroups stm text time
    transformers websockets
  ];
  description = "client API for interacting with focus projects";
  license = stdenv.lib.licenses.unfree;
}
