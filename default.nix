{ pkgs ? import <nixpkgs> {}
, hpkgs ? pkgs.haskellPackages
, mkDerivation ? hpkgs.mkDerivation
, bases ? hpkgs.bases or import ../bases { inherit pkgs hpkgs mkDerivation; }
}: {
  haskell = hpkgs.callCabal2nix "treehash" (pkgs.lib.cleanSource ./.) { inherit mkDerivation bases; };
}
