{mkDerivation, loch-th, attoparsec, snap, text}: 

mkDerivation {
  pname = "focus-http";
  license = null;
  version = "0.1";
  src = ./.;
  buildDepends = [
    attoparsec
    snap
    text
    loch-th
  ];
  pkgconfigDepends = [
  ];
}
