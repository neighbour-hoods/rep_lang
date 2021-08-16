let
  sources = import ./nix/sources.nix;
  rust-overlay = import sources.rust-overlay;
  cargo2nix = (import sources.cargo2nix { }).package;
in

{ system ? builtins.currentSystem
, pkgs ? import sources.nixpkgs {
    inherit system;
    overlays = [
      rust-overlay
    ];
  }
}:

pkgs.mkShell {
  buildInputs = with pkgs; [
    cargo2nix
    rust-bin.stable.latest.default
  ];
}
