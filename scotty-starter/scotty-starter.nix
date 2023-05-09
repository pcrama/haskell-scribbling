{ mkDerivation, aeson, base, blaze-html, blaze-markup, clay, lib
, scotty, text, wai-extra, wai-middleware-static
}:
mkDerivation {
  pname = "app";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base blaze-html blaze-markup clay scotty text wai-extra
    wai-middleware-static
  ];
  homepage = "https://github.com/pcrama/haskell-scribbling/scotty-starter/";
  description = "Meter Reading Note Taking App";
  license = lib.licenses.publicDomain;
  mainProgram = "app";
}
