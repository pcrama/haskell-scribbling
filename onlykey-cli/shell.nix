{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, hidapi, stdenv }:
      mkDerivation {
        pname = "onlykey-cli";
        version = "0.1.0.1";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [ base bytestring hidapi ];
        homepage = "https://github.com/pcrama/haskell-scribbling/onlykey-cli";
        description = "Interface with OnlyKey https://onlykey.io";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
