{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
    cargo2nix.url = "github:cargo2nix/cargo2nix";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { nixpkgs, flake-utils, rust-overlay, cargo2nix, ... }:
    flake-utils.lib.eachSystem ["x86_64-linux" "aarch64-linux"] (system:
      let

        overlays = [
          (import rust-overlay)
          (import "${cargo2nix}/overlay")
        ];

        pkgs = import nixpkgs {
          inherit system overlays;
        };

        rustVersion = "1.54.0";

      in

      {

        devShell = pkgs.mkShell {
          buildInputs = [
            pkgs.rust-bin.stable.${rustVersion}.default
            cargo2nix.defaultPackage.${system}
          ];
        };

        packages.rep_lang_runtime =
          let
            rustPkgs = pkgs.rustBuilder.makePackageSet' {
              rustChannel = rustVersion;
              packageFun = import ./rep_lang_runtime/Cargo.nix;
            };
          in
          rustPkgs.workspace.rep_lang_runtime {};

      });
}
