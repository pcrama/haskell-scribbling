{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, hspec, mtl, ncurses
      , optparse-applicative, QuickCheck, stdenv
      }:
      mkDerivation {
        pname = "sokoban";
        version = "0.4.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base hspec mtl ncurses optparse-applicative QuickCheck
        ];
        homepage = "https://github.com/pcrama/haskell-scribbling/sokoban";
        description = "Sokoban clone";
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
