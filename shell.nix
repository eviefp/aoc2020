let
  tooling = import ./nix/default.nix;
  self = tooling.haskell.ghc8102;
in
  self.shell
