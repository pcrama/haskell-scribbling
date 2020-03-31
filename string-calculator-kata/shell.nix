{ nixpkgs ? import <nixpkgs> {} }:
let
  withHoogle = pkgs.lib.inNixShell;

  release = import ./release.nix { withHoogle = withHoogle; };
  inherit (release.pinnedPkgs) pkgs;
  inherit (pkgs) haskellPackages;
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = release.project.env.nativeBuildInputs ++ [
    haskellPackages.cabal-install
    haskellPackages.hlint
    haskellPackages.ghcid
    pkgs.git
  ];
}