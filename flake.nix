{
  description = "Utility for getting status of a Git repository tree";

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };

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
        overlays = [ haskellNix.overlay
          (final: prev: {
            git-all =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc910";
                index-state = "2024-11-01T00:00:00Z";
                shell = {
                  tools = {
                    cabal = {};
                    haskell-language-server = {};
                  };
                  buildInputs = with pkgs; [
                    pkg-config
                    haskellPackages.hlint
                    haskellPackages.fourmolu
                    lefthook
                  ];
                  withHoogle = false;
                };
              };
          })
        ];
        flake = pkgs.git-all.flake {};
      in flake // {
        packages = flake.packages // {
          default = flake.packages."git-all:exe:git-all";
        };

        devShells.default = flake.devShells.default;

        checks = (flake.checks or {}) // {
          build = flake.packages."git-all:exe:git-all";

          hlint = pkgs.runCommand "hlint-check" {
            nativeBuildInputs = [ pkgs.haskellPackages.hlint ];
          } ''
            hlint ${self}/Main.hs
            touch $out
          '';

          fourmolu = pkgs.runCommand "fourmolu-check" {
            nativeBuildInputs = [ pkgs.haskellPackages.fourmolu ];
          } ''
            fourmolu --mode check \
              --config ${self}/fourmolu.yaml \
              ${self}/Main.hs ${self}/test/Spec.hs
            touch $out
          '';
        };

        # Profiling configuration (uncomment in modules to enable):
        # modules = [{
        #   enableLibraryProfiling = true;
        #   enableProfiling = true;
        # }];

        apps = {
          default = {
            type = "app";
            program = "${flake.packages."git-all:exe:git-all"}/bin/git-all";
          };
          format = {
            type = "app";
            program = "${pkgs.writeShellScript "git-all-format" ''
              exec ${pkgs.haskellPackages.fourmolu}/bin/fourmolu --mode inplace "$@"
            ''}";
          };
          lint = {
            type = "app";
            program = "${pkgs.writeShellScript "git-all-lint" ''
              exec ${pkgs.haskellPackages.hlint}/bin/hlint "$@"
            ''}";
          };
        };
      });
}
