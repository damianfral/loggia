{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7103" }:
let
  haskellPackages =  nixpkgs.pkgs.haskell.packages.${compiler}.override
  {
    overrides = self: super: 
    {
      loggia-common = self.callPackage ../common/loggia-common.nix {};
    };
  };
in
  haskellPackages.callPackage ./loggia-app.nix { }
