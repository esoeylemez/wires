{ mkDerivation, base, profunctors, stdenv }:
mkDerivation {
  pname = "wires";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base profunctors ];
  homepage = "https://github.com/esoeylemez/wires";
  description = "Functional reactive programming library";
  license = stdenv.lib.licenses.bsd3;
}
