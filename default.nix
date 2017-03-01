{ compiler ? "ghcjs" }:

let

  bootstrap = import <nixpkgs> { };

  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  src = bootstrap.fetchFromGitHub
  {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };

  config =
  {
    allowBroken = true;
    packageOverrides = pkgs : rec
    {

      haskellPackages = pkgs.haskell.packages.${compiler}.override
      {
        overrides = haskellPackagesNew: haskellPackagesOld: rec
        {
          loggia-common = haskellPackagesNew.callPackage ./common/default.nix {};
          loggia-client = haskellPackagesNew.callPackage ./client/default.nix {};
        };
      };

      loggia-minimal  = pkgs.stdenv.mkDerivation 
      {
        name = "loggia-minimal";
        buildCommand = ''
          mkdir -p $out/bin
          cp ${haskellPackages.loggia-common}/bin/loggia-common $out/bin/loggia-common
          cp ${haskellPackages.loggia-client}/bin/loggia-client $out/bin/loggia-client
        '';
      };

      docker-container = pkgs.dockerTools.buildImage
      {
        name = "docker-container";
        config.Cmd = [ "${loggia-minimal}/bin/loggi-client" ];
      };
      

    };
  };

  pkgs = import src { inherit config; };


in
  {
    loggia-common = pkgs.haskellPackages.loggia-common;
    loggia-client = pkgs.haskellPackages.loggia-client;
    docker-container = pkgs.docker-container;
  }

