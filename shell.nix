with import <nixpkgs> { };

haskell.lib.buildStackProject {
  name = "stack2nix";
  ghc = haskell.packages.ghc821.ghc;
  shellHook = "export SSL_CERT_FILE=${cacert}/etc/ssl/certs/ca-bundle.crt";
  buildInputs =
    [ z3
      cabal-install
      git
      ncurses ];
}
