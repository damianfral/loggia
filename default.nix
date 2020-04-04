{ compiler ? "ghcjs" }:

let

  bootstrap = import <nixpkgs> { };

  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  reflex-platform = import ( bootstrap.fetchFromGitHub {
    owner  = "reflex-frp";
    repo   = "reflex-platform";
    rev    = "b7c00b3574d0ef42974eda0f2812c794c7b5d4f3";
    sha256 = "1jfz17y2fq051caby4y4aslxrpvgwwa30ivfw0l5wn5pp5zlrpad";
    # inherit ( builtins.fromJSON (builtins.readFile ./reflex-platform.json) ) rev sha256;
  }) {};


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

      haskellPackages = reflex-platform.${compiler}.override
      {
        overrides = self: super:
        {
          loggia-common = self.callPackage ./common/default.nix {};
          loggia-client = self.callPackage ./client/default.nix {};
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

