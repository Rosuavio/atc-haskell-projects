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

    dev() {
      cabal build

      $TERM ghciwatch --command 'cabal repl' --watch src --clear > /dev/null 2>&1 &
      sleep 2s && $TERM cabal repl > /dev/null 2>&1 &
    }
  '';
}
