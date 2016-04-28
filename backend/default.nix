{ mkDerivation, aeson, attoparsec, base, blaze-html, bytestring
, clientsession, containers, data-default, diagrams, diagrams-lib
, diagrams-svg, directory, file-embed, filepath, focus-core
, focus-serve, focus-th, groundhog, groundhog-postgresql
, groundhog-th, lens, loch-th, lucid, mime-mail, monad-control
, monad-logger, monad-loops, mtl, network, postgresql-simple
, process, pwstore-fast, raw-strings-qq, resource-pool, semigroups
, shelly, smtp-mail, snap, snap-core, stdenv, stm, stringsearch
, stripe, tar, template-haskell, temporary, text, these, time
, transformers, transformers-base, unix, websockets
, websockets-snap
}:
mkDerivation {
  pname = "focus-backend";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base blaze-html bytestring clientsession
    containers data-default diagrams diagrams-lib diagrams-svg
    directory file-embed filepath focus-core focus-serve focus-th
    groundhog groundhog-postgresql groundhog-th lens loch-th lucid
    mime-mail monad-control monad-logger monad-loops mtl network
    postgresql-simple process pwstore-fast raw-strings-qq resource-pool
    semigroups shelly smtp-mail snap snap-core stm stringsearch stripe
    tar template-haskell temporary text these time transformers
    transformers-base unix websockets websockets-snap
  ];
  description = "Project Synopsis Here";
  license = stdenv.lib.licenses.unfree;
}
