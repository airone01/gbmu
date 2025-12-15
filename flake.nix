{
  description = "much devshell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {inherit system;};
      in {
        devShells.default = pkgs.mkShell {
          # Tools that run at build time go here
          nativeBuildInputs = with pkgs; [
            pkg-config
            zig_0_15
            zls_0_15
          ];

          # Libraries linked at runtime go here
          buildInputs = with pkgs; [
            SDL2
            # SDL2_image
            # SDL2_ttf
          ];

          shellHook = ''
            export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath [pkgs.vulkan-loader pkgs.SDL2]}:$LD_LIBRARY_PATH"
          '';
        };
      }
    );
}
