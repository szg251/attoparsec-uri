# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

{
  description = "attoparsec-uri";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        src = ./.;
        compiler = "ghc944";
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskell.packages.${compiler};

        packageName = "attoparsec-uri";
      in
      {
        packages.${packageName} = haskellPackages.callCabal2nix packageName src { };

        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = [
            haskellPackages.ghc
            haskellPackages.haskell-language-server
            haskellPackages.cabal-install
            haskellPackages.fourmolu
            haskellPackages.hlint
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
        checks = self.packages.${system}.${packageName};
      });
}
