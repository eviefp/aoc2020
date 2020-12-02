let
  tooling = import ./nix/default.nix;
  self = tooling.haskell.ghc884;
in
  self.shell
