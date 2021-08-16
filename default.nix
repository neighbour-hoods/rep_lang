{
  system ? builtins.currentSystem,
}:
let
  sources = import ./nix/sources.nix;
  cargo2nix-overlay = import "${sources.cargo2nix}/overlay";
  rust-overlay = import sources.rust-overlay;

  pkgs = import sources.nixpkgs {
    inherit system;
    overlays = [
      cargo2nix-overlay
      rust-overlay
    ];
  };

  rustPkgs = pkgs.rustBuilder.makePackageSet' {
    rustChannel = "stable";
    packageFun = import ./Cargo.nix;
  };
in

rustPkgs.workspace.rep_lang_runtime {}
