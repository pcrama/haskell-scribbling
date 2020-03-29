{ withHoogle ? false
, compiler ? "ghc865"
}:
let
  pinnedPkgs = import ./pkgs-from-json.nix { json = ./nixos-19.09.json; };
  hoogleAugmentedPackages = import ./toggle-hoogle.nix { withHoogle = withHoogle; input = pinnedPkgs.haskell.packages.${compiler}; };
in
  { project = hoogleAugmentedPackages.callPackage ./default.nix { };
    pkgs = pinnedPkgs;
  }
