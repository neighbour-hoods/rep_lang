{
  inputs = {
    nh-nix-env.url = "github:neighbour-hoods/nh-nix-env";
  };

  outputs = { nh-nix-env, ... }:
    let
      flake-utils = nh-nix-env.metavalues.flake-utils;
      nh-supported-systems = nh-nix-env.metavalues.nh-supported-systems;
      rustVersion = nh-nix-env.metavalues.rustVersion;
      naersk = nh-nix-env.metavalues.naersk;
    in
    flake-utils.lib.eachSystem nh-supported-systems (system:
      let
        pkgs = nh-nix-env.values.${system}.pkgs;
      in
      {
        devShells.default = nh-nix-env.shells.${system}.rustDevShell {};

        # warning: this is broken due to
        # https://github.com/nix-community/naersk/issues/133
        # packages.rep_lang_runtime =
        #   let
        #     rust = pkgs.rust-bin.stable.${rustVersion}.default;
        #
        #     naersk' = pkgs.callPackage naersk {
        #       cargo = rust;
        #       rustc = rust;
        #     };
        #   in
        #   naersk'.buildPackage {
        #     src = ./rep_lang_runtime;
        #     copyLibs = true;
        #   };

        # warning: this is broken due to
        # https://github.com/cargo2nix/cargo2nix/issues/211
        # packages.rep_lang_runtime-cargo2nix =
        #   let
        #     rustPkgs = pkgs.rustBuilder.makePackageSet {
        #       rustChannel = rustVersion;
        #       packageFun = import ./rep_lang_runtime/Cargo.nix;
        #     };
        #   in
        #   rustPkgs.workspace.rep_lang_runtime {};

      });
}
