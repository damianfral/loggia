{ mkDerivation, aeson, base, basic-prelude, bytestring, containers
, directory, fixplate, lens, lucid, MonadRandom, mtl, pipes, random
, semigroups, stdenv, svg-tree, text, transformers, turtle
, unordered-containers
}:
mkDerivation {
  pname = "loggia-common";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base basic-prelude bytestring containers directory fixplate
    lens lucid MonadRandom mtl pipes random semigroups svg-tree text
    transformers turtle unordered-containers
  ];
  homepage = "http://github.com/damianfral/loggia";
  description = "A gallery generator";
  license = stdenv.lib.licenses.mit;
}
