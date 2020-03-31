{ mkDerivation, base, hspec, stdenv }:
mkDerivation {
  pname = "string-calculator-kata";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  homepage = "https://github.com/pcrama/haskell-scribbling/string-calculator-kata";
  description = "TDD exercise: https://osherove.com/tdd-kata-1";
  license = "LicenseRef-PublicDomain";
}
