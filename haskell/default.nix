{ mkDerivation, aeson, async, base, bytestring, contravariant
, hasql, hasql-pool, interpolatedstring-perl6, monad-logger, mtl
, rainbow, stdenv, template-haskell, text, time, transformers
, unliftio-core, uuid
}:
mkDerivation {
  pname = "contextual-logger";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base bytestring contravariant hasql hasql-pool
    interpolatedstring-perl6 monad-logger mtl rainbow template-haskell
    text time transformers unliftio-core uuid
  ];
  executableHaskellDepends = [
    aeson async base bytestring text time transformers
  ];
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
