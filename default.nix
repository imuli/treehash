{ pkgs ? import <nixpkgs> {}, hpkgs ? pkgs.haskellPackages, mkDerivation ? hpkgs.mkDerivation }:
let bases = import ../bases { inherit pkgs hpkgs mkDerivation; };
 in {
   haskell = hpkgs.callCabal2nix "treehash" (pkgs.lib.cleanSource ./.) { inherit mkDerivation bases; };
 }
