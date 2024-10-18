{
  description = "Automatically sets the dark-mode theme based on macOS/Linux/Windows status";

  nixConfig = {
    ## https://github.com/NixOS/rfcs/blob/master/rfcs/0045-deprecate-url-syntax.md
    extra-experimental-features = ["no-url-literals"];
    extra-substituters = ["https://cache.garnix.io"];
    extra-trusted-public-keys = [
      "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
    ];
    ## Isolate the build.
    sandbox = "relaxed";
    use-registries = false;
  };

  outputs = {
    bash-strict-mode,
    flake-utils,
    flaky,
    nixpkgs,
    self,
    systems,
  }: let
    pname = "auto-dark";

    supportedSystems = import systems;
  in
    {
      schemas = {
        inherit
          (flaky.schemas)
          overlays
          homeConfigurations
          packages
          devShells
          checks
          formatter
          ;
      };

      overlays = {
        default = flaky.lib.elisp.overlays.default self.overlays.emacs;

        emacs = final: prev: efinal: eprev: {inherit (self.packages.${final.system}) auto-dark;};
      };

      homeConfigurations =
        builtins.listToAttrs
        (builtins.map
          (flaky.lib.homeConfigurations.example self [./example/home.nix])
          supportedSystems);
    }
    // flake-utils.lib.eachSystem supportedSystems (system: let
      pkgs = nixpkgs.legacyPackages.${system}.appendOverlays [
        flaky.overlays.dependencies
        flaky.overlays.elisp-dependencies
      ];

      src = pkgs.lib.cleanSource ./.;
    in {
      packages =
        {
          default = self.packages.${system}.auto-dark;
          auto-dark = pkgs.callPackage ./nix/packages/auto-dark.nix {
            inherit src;
            inherit (pkgs) emacsPackages;
            checkedDrv = bash-strict-mode.lib.checkedDrv pkgs;
          };
        }
        // nixpkgs.lib.listToAttrs (nixpkgs.lib.concatMap (v:
          if nixpkgs.lib.elem system pkgs.${v}.meta.platforms
          then [
            {
              name = "${v}_auto-dark";
              value = pkgs.callPackage ./nix/packages/auto-dark.nix {
                inherit src;
                checkedDrv = bash-strict-mode.lib.checkedDrv pkgs;
                emacsPackages = pkgs.emacsPackagesFor pkgs.${v};
              };
            }
          ]
          else []) [
          ## Emacs 28 doesn’t like setting up frames in batch mode, so only test
          ## on Emacs 29 for now.
          "emacs29"
          "emacs29-gtk3"
          "emacs29-macport"
          "emacs29-nox"
          "emacs29-pgtk"
        ]);

      devShells.default = bash-strict-mode.lib.checkedDrv pkgs (pkgs.mkShell {
        inherit (pkgs) system;
        inputsFrom =
          builtins.attrValues self.checks.${system}
          ++ builtins.attrValues self.packages.${system};
      });

      checks = import ./nix/checks.nix {
        inherit src;
        inherit (pkgs) emacsPackages emacsWithPackages stdenv;
        checkedDrv = bash-strict-mode.lib.checkedDrv pkgs;
      };

      formatter = pkgs.alejandra;
    });

  inputs = {
    ## Flaky should generally be the source of truth for its inputs.
    flaky.url = "github:sellout/flaky";

    bash-strict-mode.follows = "flaky/bash-strict-mode";
    flake-utils.follows = "flaky/flake-utils";
    nixpkgs.follows = "flaky/nixpkgs";
    systems.follows = "flaky/systems";
  };
}
