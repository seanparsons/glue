{
  description = "glue: Combinator library for building better services and clients in Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
  };

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      supportedGhcs = [
        "ghc96"
        "ghc98"
        "ghc910"
        "ghc912"
      ];

      defaultGhc = "ghc910";

      haskellOverlay = final: prev: {
        glue-common = final.callCabal2nix "glue-common" ./glue-common { };
        glue-core = final.callCabal2nix "glue-core" ./glue-core { };
        glue-ekg = final.callCabal2nix "glue-ekg" ./glue-ekg { };
        glue-example = final.callCabal2nix "glue-example" ./glue-example { };
      };

      mkHaskellPackages = pkgs: ghcVer:
        pkgs.haskell.packages.${ghcVer}.override {
          overrides = haskellOverlay;
        };
    in
    {
      overlays.default = final: prev: {
        haskell = prev.haskell // {
          packages = prev.haskell.packages // nixpkgs.lib.genAttrs supportedGhcs (ghcVer:
            prev.haskell.packages.${ghcVer}.override {
              overrides = haskellOverlay;
            }
          );
        };
        haskellPackages = prev.haskellPackages.override {
          overrides = haskellOverlay;
        };
      };

      packages = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
          defaultHp = mkHaskellPackages pkgs defaultGhc;

          mkGhcPackages = ghcVer:
            let
              hp = mkHaskellPackages pkgs ghcVer;
              allForGhc = pkgs.symlinkJoin {
                name = "glue-${ghcVer}-all";
                paths = [
                  hp.glue-common
                  hp.glue-core
                  hp.glue-ekg
                  hp.glue-example
                ];
              };
            in
            {
              "${ghcVer}-glue-common" = hp.glue-common;
              "${ghcVer}-glue-core" = hp.glue-core;
              "${ghcVer}-glue-ekg" = hp.glue-ekg;
              "${ghcVer}-glue-example" = hp.glue-example;
              "${ghcVer}-all" = allForGhc;
            };

          perGhcPackages = builtins.foldl' (acc: ghcVer: acc // mkGhcPackages ghcVer) { } supportedGhcs;
        in
        perGhcPackages // {
          glue-common = defaultHp.glue-common;
          glue-core = defaultHp.glue-core;
          glue-ekg = defaultHp.glue-ekg;
          glue-example = defaultHp.glue-example;
          default = perGhcPackages."${defaultGhc}-all";
          all-ghcs = pkgs.symlinkJoin {
            name = "glue-all-ghcs";
            paths = map (ghcVer: perGhcPackages."${ghcVer}-all") supportedGhcs;
          };
        }
      );

      apps = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
          defaultHp = mkHaskellPackages pkgs defaultGhc;
        in
        {
          default = {
            type = "app";
            program = "${defaultHp.glue-example}/bin/glue-example";
          };
        }
      );

      checks = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
          mkGhcChecks = ghcVer:
            let
              hp = mkHaskellPackages pkgs ghcVer;
            in
            {
              "${ghcVer}-glue-common" = hp.glue-common;
              "${ghcVer}-glue-core" = hp.glue-core;
              "${ghcVer}-glue-ekg" = hp.glue-ekg;
              "${ghcVer}-glue-example" = hp.glue-example;
            };
        in
        builtins.foldl' (acc: ghcVer: acc // mkGhcChecks ghcVer) { } supportedGhcs
      );

      devShells = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
          mkShellFor = ghcVer:
            let
              hp = mkHaskellPackages pkgs ghcVer;
            in
            hp.shellFor {
              packages = p: [
                p.glue-common
                p.glue-core
                p.glue-ekg
                p.glue-example
              ];
              withHoogle = false;
              nativeBuildInputs = [
                pkgs.cabal-install
                pkgs.hlint
                hp.hspec-discover
              ] ++ nixpkgs.lib.optionals (hp ? haskell-language-server) [
                hp.haskell-language-server
              ] ++ nixpkgs.lib.optionals (hp ? fourmolu) [
                hp.fourmolu
              ];
            };
          perGhcShells = nixpkgs.lib.genAttrs supportedGhcs mkShellFor;
        in
        perGhcShells // {
          default = perGhcShells.${defaultGhc};
        }
      );
    };
}
