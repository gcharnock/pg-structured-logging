{ mkDerivation, async, base, bytestring, contravariant, hasql
, hasql-pool, monad-logger, mtl, rainbow, stdenv, text, time
, transformers, unliftio, unliftio-core
}:
mkDerivation {
  pname = "evnestlog";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base bytestring contravariant hasql hasql-pool monad-logger
    mtl rainbow text time transformers unliftio unliftio-core
  ];
  executableHaskellDepends = [
    async base bytestring hasql hasql-pool text time
  ];
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
