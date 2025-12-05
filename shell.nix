let
  sources = import ./npins;
  pkgs = import sources.nixpkgs {};
in pkgs.mkShell {
  packages = [
    pkgs.haskellPackages.ghc
    pkgs.haskellPackages.cabal-install
    pkgs.haskellPackages.haskell-language-server
    pkgs.npins
    pkgs.ghciwatch
  ];

  shellHook = ''
    cabal update
  '';
}
