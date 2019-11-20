{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, sdl2, sdl2-image, sdl2-ttf, stdenv, text }:
mkDerivation {
  pname = "fist-sdl2";
  version = "0.0.0.3";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base sdl2 sdl2-image sdl2-ttf text ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
};

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
