{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

# nix-shell -p cabal2nix --command "cabal2nix --shell '$PWD'" > shell.nix
# edit manually to add cabal-install to input args & executableHaskellDepends

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, containers, hspec, lib, mtl
      , parsec, text, time, vector, cabal-install
      }:
      mkDerivation {
        pname = "autoledger";
        version = "0.1.0.0";
        src = /home/philippe/prog/haskell-scribbling/autoledger;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base mtl parsec text time ];
        executableHaskellDepends = [
          base bytestring containers text time vector cabal-install
        ];
        testHaskellDepends = [ base hspec mtl parsec text time ];
        description = "Translate Belfius CSV export into initial ledger input";
        license = "unknown";
        mainProgram = "autoledger";
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
