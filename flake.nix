# This file is pretty general, and you can adapt it in your project replacing
# only `name` and `description` below.

{
  description = "HyprParse dev shell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
		rust-overlay = {
     url = "github:oxalica/rust-overlay";
     inputs.nixpkgs.follows = "nixpkgs";
    };
    nci.url = "github:yusdacra/nix-cargo-integration";
    nci.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs:
    inputs.nci.lib.makeOutputs {
      root = ./.;
      config = common: {
        shell = {
					env = [
						{
							name = "PATH";
							eval = "$HOME/.cargo/bin:$PATH";
						}
					];
          packages = with common.pkgs; [ 
						#rust-analyzer
						wasm-pack
						wasm-bindgen-cli
						nodePackages.npm
					];
        };
      };
    };
}
