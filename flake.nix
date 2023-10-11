{
  description = "Solver for the 3-partition problem";

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
      flake = pkgs.three-partition.flake {
      };
      overlays = [ haskellNix.overlay
        (final: prev: {
          three-partition =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc963";
              shell.tools = {
                cabal = {};
                # haskell-language-server = {};
                # hlint = {};
              };
              shell.buildInputs = with pkgs; [
                pkg-config
              ];
            };
        })
      ];
    in flake // {
      packages.default = flake.packages."three-partition:exe:three-partition";

      devShell = pkgs.haskellPackages.shellFor {
        packages = p: [
        ];

        buildInputs = with pkgs.haskellPackages; [
          cabal-install
        ];

        withHoogle = true;
      };
    });
}
