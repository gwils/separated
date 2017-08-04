{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  separated = pkgs.haskell.lib.dontCheck (haskellPackages.callPackage ./separated.nix {});

in

  separated

