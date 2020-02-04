with import <nixpkgs> {};
stdenv.mkDerivation rec {
	name = "dhall-text";
	env = buildEnv { name = name; paths = buildInputs; };
	buildInputs = [
		dhall-text
	];
}

