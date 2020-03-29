{
  # Whether or not hoogle should be enabled.
  withHoogle
  # Input set of all haskell packages. A valid input would be:
  # (import <nixpkgs> {}).pkgs.haskellPackages or (import <nixpkgs> {}).pkgs.haskell.packages.ghc844
, input
}:
if withHoogle
  then  input.override {
          overrides = (self: super:
            {
              ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
              ghcWithPackages = self.ghc.withPackages;
            }
          );
        }
  else  input
