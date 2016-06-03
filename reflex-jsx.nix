{ mkDerivation, base, containers, haskell-src-meta, parsec, reflex
, reflex-dom, stdenv, template-haskell, text
}:
mkDerivation {
  pname = "reflex-jsx";
  version = "0.1.0.0";
  sha256 = "1r7xk0gd7caligpmlqb16sv2wrn2z021x307zdk5ksq066x02s50";
  libraryHaskellDepends = [
    base containers haskell-src-meta parsec reflex reflex-dom
    template-haskell text
  ];
  homepage = "https://github.com/dackerman/reflex-jsx";
  description = "Use jsx-like syntax in Reflex";
  license = stdenv.lib.licenses.bsd3;
}
