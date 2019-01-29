{ mkDerivation, base, bytestring, hasql, monad-logger, mtl, rainbow
, stdenv, text, transformers, unliftio, unliftio-core
}:
mkDerivation {
  pname = "evnestlog";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring hasql monad-logger mtl rainbow transformers
    unliftio unliftio-core
  ];
  executableHaskellDepends = [ base bytestring text ];
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
