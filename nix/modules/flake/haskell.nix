# haskell-flake configuration goes in this module.

{ root, inputs, ... }:
{
  imports = [
    inputs.haskell-flake.flakeModule
  ];
  perSystem =
    {
      self',
      lib,
      config,
      pkgs,
      ...
    }:
    {
      # Our only Haskell project. You can have multiple projects, but this template
      # has only one.
      # See https://github.com/srid/haskell-flake/blob/master/example/flake.nix
      haskellProjects.default = {
        projectRoot = builtins.toString (
          lib.fileset.toSource {
            inherit root;
            fileset = lib.fileset.unions [
              (root + /app)
              (root + /src)
              (root + /test)
              (root + /oke.cabal)
              (root + /LICENSE)
              (root + /README.md)
            ];
          }
        );

        # The base package set (this value is the default)
        basePackages = pkgs.haskell.packages.ghc910;

        # Packages to add on top of `basePackages`
        packages = {
          # Add source or Hackage overrides here
          # (Local packages are added automatically)
          /*
            aeson.source = "1.5.0.0" # Hackage version
            shower.source = inputs.shower; # Flake input
          */
        };

        # Add your package overrides here
        settings = {
          oke = {
            stan = true;
            # check = true;
          };
        };

        # What should haskell-flake add to flake outputs?
        autoWire = [
          "packages"
          "apps"
          "checks"
        ]; # Wire all but the devShell
      };

      # Default package & app.
      packages.default = pkgs.haskell.lib.justStaticExecutables self'.packages.oke;
      apps.default = self'.apps.oke;
    };
}
