{mkDerivation, groundhog, groundhog-th, mtl, focus-core, focus-serve, lens, aeson, snap, resource-pool, text, network, stm, postgresql-simple, groundhog-postgresql, websockets-snap, websockets, smtp-mail, temporary, stringsearch, shelly, tar, file-embed, binary, lucid, diagrams, diagrams-lib, diagrams-svg, raw-strings-qq, attoparsec, focus-th, focus-aeson-orphans, mustache, parsec, vector, word8, myPostgres}:

mkDerivation {
  pname = "focus-backend";
  license = null;
  version = "0.1";
  src = ./.;
  buildDepends = [
    groundhog
    groundhog-th
    mtl
    focus-core
    focus-th
    focus-serve
    focus-aeson-orphans
    lens
    aeson
    snap
    resource-pool
    text
    network
    stm
    postgresql-simple
    groundhog-postgresql
    websockets-snap
    websockets
    smtp-mail
    temporary
    stringsearch
    shelly
    tar
    file-embed
    binary
    lucid
    diagrams
    diagrams-lib
    diagrams-svg
    raw-strings-qq
    attoparsec
    mustache
    parsec
    vector
    word8
  ];
  pkgconfigDepends = [
    myPostgres
  ];
}
