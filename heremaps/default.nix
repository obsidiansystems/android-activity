{ mkDerivation, aeson, base, focus-core, http-conduit, network-uri
, stdenv, text, time
}:
mkDerivation {
  pname = "focus-heremaps";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base focus-core http-conduit network-uri text time
  ];
  description = "Project Synopsis Here";
  license = stdenv.lib.licenses.unfree;
}
