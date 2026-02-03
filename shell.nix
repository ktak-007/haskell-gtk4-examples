{ sources ? import ./nix/sources.nix
}:
let pkgs = import sources.nixpkgs {};
in pkgs.haskellPackages.developPackage {
  root = ./.;
  returnShellEnv = true;
  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
      [ cabal-install
        haskell-language-server
      ]);
}
