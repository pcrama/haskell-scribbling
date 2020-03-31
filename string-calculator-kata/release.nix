{ withHoogle ? false }:
let
  nixpkgs = import <nixpkgs> { }; # this is where we would normally pin the packages
  bareHaskellPackages = nixpkgs.pkgs.haskellPackages; # or nixpkgs.pkgs.haskell.packages.ghc844
  haskellPackages = if withHoogle
                    then bareHaskellPackages.override {
                           overrides = (self: super:
                             {
                               ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
                               ghcWithPackages = self.ghc.withPackages;
                             }
                           );
                         }
                    else bareHaskellPackages;
in
   { project = haskellPackages.callPackage ./default.nix {};
     pinnedPkgs = nixpkgs;
   }
