{ mkDerivation, stdenv, array, base, deepseq, ghc-prim
}:
mkDerivation {
  pname = "containers";
  version = "0.6.0.1";
  sha256 = "a71197d356e578651c37986493ff999f54773169acc32e5997c8248aca0bac6a";
  libraryHaskellDepends = [ array base deepseq ghc-prim ];
  doCheck = false;
  description = "Assorted concrete container types";
  license = stdenv.lib.licenses.bsd3;
}
