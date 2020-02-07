let 
  nixpkgs = import <nixpkgs> {};
in
  nixpkgs.stdenv.mkDerivation rec {
    name = "dhall";
    env = nixpkgs.buildEnv { name = name; paths = buildInputs; };
    buildInputs = [
      nixpkgs.dhall
    ];
  }

