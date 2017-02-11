{ mkDerivation, attoparsec, base, base-compat, base-orphans
, base16-bytestring, bytestring, containers, deepseq, directory
, dlist, filepath, generic-deriving, ghc-prim, hashable
, hashable-time, HUnit, QuickCheck, quickcheck-instances
, scientific, stdenv, tagged, template-haskell, test-framework
, test-framework-hunit, test-framework-quickcheck2, text, time
, time-locale-compat, unordered-containers, uuid-types, vector
}:
mkDerivation {
  pname = "aeson";
  version = "1.1.0.0";
  sha256 = "1048y5spv79lvkpgb23yl9h3xsbf2zyzy6fiaxjblma8crgzq42q";
  libraryHaskellDepends = [
    attoparsec base base-compat bytestring containers deepseq dlist
    ghc-prim hashable scientific tagged template-haskell text time
    time-locale-compat unordered-containers uuid-types vector
  ];
  testHaskellDepends = [
    attoparsec base base-compat base-orphans base16-bytestring
    bytestring containers directory dlist filepath generic-deriving
    ghc-prim hashable hashable-time HUnit QuickCheck
    quickcheck-instances scientific tagged template-haskell
    test-framework test-framework-hunit test-framework-quickcheck2 text
    time time-locale-compat unordered-containers uuid-types vector
  ];
  homepage = "https://github.com/bos/aeson";
  description = "Fast JSON parsing and encoding";
  license = stdenv.lib.licenses.bsd3;
  doCheck = false;
  # Export functions used by TH splices so they can be used when TH is unavailable
  preConfigure = ''
    sed -i "s/^ *, defaultTaggedObject\$/\0, keyValuePairWith, lookupField, parseTypeMismatch', valueConName/" Data/Aeson/TH.hs
  '';
}
