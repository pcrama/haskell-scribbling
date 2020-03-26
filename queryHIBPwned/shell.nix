# enforce "ghc844" instead of "default" to be compatible with the
# narrow version bounds of the libraries in the cabal file (this
# project is sometimes also developed on an Ubuntu machine where this
# is still the GHC)
{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc844", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, cabal-install, hspec, http-client-tls
      , http-types, megaparsec, mtl, SHA, stdenv, text
      }:
      mkDerivation {
        pname = "queryHIBPwned";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base bytestring megaparsec SHA text ];
        executableHaskellDepends = [
          base bytestring http-client-tls http-types megaparsec SHA text
        ];
        testHaskellDepends = [ base bytestring hspec megaparsec mtl text ];
        homepage = "https://github.com/pcrama/haskell-scribbling/queryHIBPwned";
        description = "Query HaveIBeenPwned with information parsed from .netrc";
        license = stdenv.lib.licenses.bsd3;
        buildDepends = [ cabal-install ];
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
