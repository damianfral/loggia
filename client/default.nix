{ mkDerivation, aeson, base, basic-prelude, bytestring, containers
, data-default, dependent-sum, either, file-embed, fixplate
, ghcjs-base, ghcjs-dom, lens, loggia-common, MonadRandom, mtl
, ref-tf, reflex, reflex-dom, semigroups, stdenv, text
, transformers, vector
}:
mkDerivation {
  pname = "loggia-client";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base basic-prelude bytestring containers data-default
    dependent-sum either file-embed fixplate ghcjs-base ghcjs-dom lens
    loggia-common MonadRandom mtl ref-tf reflex reflex-dom semigroups
    text transformers vector
  ];
  license = stdenv.lib.licenses.gpl3;
}
