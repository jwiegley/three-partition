{
  description = "Solver for the 3-partition problem";

  nixConfig = {
    allow-import-from-derivation = true;
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
          three-partition =
            final.haskell-nix.project' {
              src = ./.;
              supportHpack = true;
              compiler-nix-name = "ghc910";

              modules = [{
                packages.three-partition = {
                  ghcOptions = [ "-Werror" ];
                };
              }];

              shell = {
                tools = {
                  cabal = {};
                  haskell-language-server = {};
                  hlint = {};
                  fourmolu = {};
                };
                buildInputs = with pkgs; [
                  pkg-config
                  lefthook
                  z3
                ];
              };
            };
        })
      ];

      project = pkgs.three-partition;
      flake = project.flake {};

      hsSources = [
        "${self}/src/ThreePartition.hs"
        "${self}/app/Main.hs"
        "${self}/test/Spec.hs"
        "${self}/bench/Main.hs"
      ];
      hsSourcesStr = builtins.concatStringsSep " " hsSources;

    in flake // {
      packages = flake.packages // {
        default = flake.packages."three-partition:exe:three-partition";
      };

      checks = (flake.checks or {}) // {
        build = flake.packages."three-partition:exe:three-partition";

        format = pkgs.runCommand "format-check" {
          nativeBuildInputs = [ pkgs.haskellPackages.fourmolu ];
        } ''
          fourmolu --config ${self}/fourmolu.yaml --mode check ${hsSourcesStr}
          touch $out
        '';

        lint = pkgs.runCommand "lint-check" {
          nativeBuildInputs = [ pkgs.haskellPackages.hlint ];
        } ''
          hlint ${hsSourcesStr}
          touch $out
        '';
      };

      devShells.default = project.shell;
    });
}
