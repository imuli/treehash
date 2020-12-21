{ pkgs ? import <nixpkgs> {}, hpkgs ? pkgs.haskellPackages }:
let mkDerivation = expr: hpkgs.mkDerivation (expr // { enableSeparateDocOutput = true; doHaddock = true; });
    self = import ./default.nix { inherit pkgs hpkgs mkDerivation; };
    extra = hpkgs.mkDerivation {
      pname = "extra"; version = "0.1.0.0"; license = "none";
      libraryToolDepends = [ pkgs.nodejs ];
    };
 in hpkgs.shellFor {
      packages = p: [ self.haskell extra ];
      withHoogle = true;
    }
