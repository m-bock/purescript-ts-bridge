{
  description = "purescript-typescript-bridge";

  inputs.nixpkgs.url = "nixpkgs";

  inputs.easy-purescript-nix = {
    url = github:justinwoo/easy-purescript-nix;
    flake = false;
  };

  inputs.flake-utils.url = github:numtide/flake-utils;

  outputs = inputs:
    inputs.flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        overlays = [
          (prev: final: {
            inherit (easy-purescript-nix) purs-tidy;
            purescript = easy-purescript-nix.purs-0_15_4;
          })
        ];

        pkgs = import
          inputs.nixpkgs
          {
            inherit system;
            inherit overlays;
          };

        easy-purescript-nix = import inputs.easy-purescript-nix { inherit pkgs; };

      in

      {
        devShell = pkgs.mkShell {
          nativeBuildInputs = [
            pkgs.purescript
            pkgs.purs-tidy
            pkgs.bashInteractive
            pkgs.spago
          ];
        };
      }
    );
}
