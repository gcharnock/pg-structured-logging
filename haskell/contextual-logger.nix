{ mkDerivation, aeson, async, base, bytestring, contravariant
, hasql, hasql-pool, interpolatedstring-perl6, monad-logger, mtl
, neat-interpolation, rainbow, stdenv, template-haskell, text, time
, transformers, unliftio, unliftio-core, uuid
}:
mkDerivation {
  pname = "contextual-logger";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base bytestring contravariant hasql hasql-pool
    interpolatedstring-perl6 monad-logger mtl neat-interpolation
    rainbow template-haskell text time transformers unliftio
    unliftio-core uuid
  ];
  executableHaskellDepends = [
    aeson async base bytestring hasql hasql-pool
    interpolatedstring-perl6 neat-interpolation text time
  ];
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
