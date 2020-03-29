let
  release = (import ./release.nix { withHoogle = true; });
  pkgs = release.pkgs; # these are actually the pinnedPkgs
  shellDrv = release.project.env.overrideAttrs (oldAttrs: rec {
    buildInputs = oldAttrs.buildInputs ++ [
      # required!
      pkgs.haskellPackages.cabal-install
      # optional
      pkgs.haskellPackages.hlint
      # pkgs.haskellPackages.hsimport # broken?
      ];
    # shellHook = ''
    #   export USERNAME="christian.henry"
    # '';
  });
in
  if pkgs.lib.inNixShell then shellDrv else release.project
