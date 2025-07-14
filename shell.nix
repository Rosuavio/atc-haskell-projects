# shell.nix
{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskellPackages.ghc
    pkgs.haskellPackages.cabal-install
  ];
  nativeBuildInputs = [
    pkgs.haskellPackages.haskell-language-server
    pkgs.ghciwatch
  ];

  shellHook = ''
    cabal update
  '';
}
