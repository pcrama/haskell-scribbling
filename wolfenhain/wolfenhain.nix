# from https://haskell4nix.readthedocs.io/nixpkgs-users-guide.html#how-to-create-nix-builds-for-your-own-private-haskell-packages
# nix-shell --pure -p cabal2nix --run "cabal2nix ." > wolfenhain.nix
#
# Then edit to add SDL2 everywhere there is already an sdl2 and this line:
#
#     pkgconfigDepends = [ SDL2 ];
{ mkDerivation, array, base, hspec, QuickCheck, SDL2, sdl2, SDL2_image, sdl2-image
, SDL2_ttf, sdl2-ttf, stdenv, text, pkgconfig
}:
mkDerivation {
  pname = "wolfenhain";
  version = "0.0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  pkgconfigDepends = [ SDL2 SDL2_image SDL2_ttf ];
  libraryHaskellDepends = [
    array base SDL2 sdl2 SDL2_image sdl2-image SDL2_ttf sdl2-ttf text
  ];
  executableHaskellDepends = [
    array base hspec QuickCheck SDL2 sdl2 SDL2_image sdl2-image SDL2_ttf sdl2-ttf text
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
