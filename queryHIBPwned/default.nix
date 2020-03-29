# cabal2nix .
{ mkDerivation, base, bytestring, hspec, http-client
, http-client-tls, http-types, megaparsec, mtl, SHA, stdenv, text
}:
mkDerivation {
  pname = "queryHIBPwned";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base bytestring megaparsec SHA text ];
  executableHaskellDepends = [
    base bytestring http-client http-client-tls http-types megaparsec
    SHA text
  ];
  testHaskellDepends = [ base bytestring hspec megaparsec mtl text ];
  homepage = "https://github.com/pcrama/haskell-scribbling/queryHIBPwned";
  description = "Query HaveIBeenPwned with information parsed from .netrc";
  license = stdenv.lib.licenses.bsd3;
}
