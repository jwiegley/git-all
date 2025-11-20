{
  description = "Utility for getting status of a Git repository tree";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.git-all.flake {
        };
        overlays = [ haskellNix.overlay
          (final: prev: {
            git-all =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc910";
                # Use a Hackage index state with compatible package versions
                index-state = "2024-11-01T00:00:00Z";
                # The cabal.project file contains allow-newer for parallel-io:containers
                # to enable building with GHC 9.10 (which ships with containers 0.7)
                shell = {
                  tools = {
                    cabal = {};
                    haskell-language-server = {};
                  };
                  buildInputs = with pkgs; [
                    pkg-config
                  ];
                  # Disable hoogle to avoid warp/http2 compilation issues
                  withHoogle = false;
                };
              };
          })
        ];
      in flake // {
        packages.default = flake.packages."git-all:exe:git-all";
        devShells.default = flake.devShells.default;
      });
}
