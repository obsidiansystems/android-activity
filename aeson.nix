{ mkDerivation, attoparsec, base, base-orphans, bytestring
, containers, deepseq, dlist, fail, ghc-prim, hashable, HUnit, mtl
, QuickCheck, quickcheck-instances, scientific, semigroups, stdenv
, syb, tagged, template-haskell, test-framework
, test-framework-hunit, test-framework-quickcheck2, text, time
, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "aeson";
  version = "0.11.2.0";
  sha256 = "0pi8s9zwirhg00q91sxg429hm22s24c5kg2r7fgcmmmqa55layj4";
  revision = "1";
  editedCabalFile = "680affa9ec12880014875ce8281efb2407efde69c30e9a82654e973e5dc2c8a1";
  libraryHaskellDepends = [
    attoparsec base bytestring containers deepseq dlist fail ghc-prim
    hashable mtl scientific semigroups syb tagged template-haskell text
    time transformers unordered-containers vector
  ];
  testHaskellDepends = [
    attoparsec base base-orphans bytestring containers ghc-prim
    hashable HUnit QuickCheck quickcheck-instances semigroups tagged
    template-haskell test-framework test-framework-hunit
    test-framework-quickcheck2 text time unordered-containers vector
  ];
  homepage = "https://github.com/bos/aeson";
  description = "Fast JSON parsing and encoding";
  license = stdenv.lib.licenses.bsd3;
}
