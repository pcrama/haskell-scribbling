{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.shellFor {
  packages = hpkgs: [
     (hpkgs.callPackage ./default.nix { })
  ];

  # development tools we use
  nativeBuildInputs = [
    pkgs.cabal-install
    pkgs.haskellPackages.doctest
    pkgs.cabal2nix
    pkgs.ghcid  # use ghcid --command "cabal repl Main.hs" --test Main.main
  ];
}
