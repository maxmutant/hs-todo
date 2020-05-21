{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.callCabal2nix "hs-snake" ./. {}
