{ pkgs ? import <nixpkgs> {} }:

let
  compiler = pkgs.haskell.packages.ghc865;
in
  compiler.developPackage {
    root = ./.;
    name = "hs-todo";
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
        [ cabal-install
        ]);
  }
