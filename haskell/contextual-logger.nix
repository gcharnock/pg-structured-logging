
let
  doJailbreak = pkgs.haskell.lib.doJailbreak;
  dontCheck = pkgs.haskell.lib.dontCheck;
  config = {
    allowUnfree = true;
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          ghc863 = pkgs.haskell.packages.ghc863.override {
            overrides = self: super: {
              contextual-logger = self.callPackage ./. {};
            };
          };
          ghc844 = pkgs.haskell.packages.ghc844.override {
            overrides = self: super: {
              contextual-logger = self.callPackage ./. {};
            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };
in
# pkgs.haskell.packages.ghc863.contextual-logger
pkgs.haskell.packages.ghc844.contextual-logger
