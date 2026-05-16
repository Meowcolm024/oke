# Configuration for the project's Nix devShell
# You mostly want the `packages` option below.

{
  perSystem =
    { config, pkgs, ... }:
    {
      # Default shell.
      devShells.default = pkgs.mkShell {
        name = "oke";
        meta.description = "Haskell development environment";

        # See https://community.flake.parts/haskell-flake/devshell#composing-devshells
        inputsFrom = [
          config.haskellProjects.default.outputs.devShell
        ];

        # Packages to be added to Nix devShell go here.
        packages = with pkgs; [
          nil
          nix-output-monitor
          pkg-config
          zlib
          libplist
        ];
      };
    };
}
