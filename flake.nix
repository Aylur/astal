{
  outputs =
    {
      self,
      nixpkgs,
    }:
    let
      forAllSystems = nixpkgs.lib.genAttrs [
        "x86_64-linux"
        "aarch64-linux"
      ];
      selfFor = system: import ./. { inherit system nixpkgs; };
    in
    {
      packages = forAllSystems (system: (selfFor system).packages);
      devShells = forAllSystems (system: (selfFor system).shells);
    };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };
}
