
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
              entropy = self.callPackage ./nix/entropy.nix { };
              hspec-core = doJailbreak (self.callPackage ./nix/hspec-core.nix { });
              hspec = super.callPackage ./nix/hspec.nix { };
              unliftio-core = doJailbreak (self.callPackage ./nix/unliftio-core.nix { });
              contextual-logger = self.callPackage ./nix/contextual-logger.nix {};
              contravariant = doJailbreak (self.callPackage ./nix/contravariant.nix {});
              lens-family-core = doJailbreak super.lens-family-core;
              doctest = doJailbreak super.doctest;
            };
          };
          ghc844 = pkgs.haskell.packages.ghc844.override {
            overrides = self: super: {
              contextual-logger = self.callPackage ./nix/contextual-logger.nix {};
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
