{ mkDerivation, base, fetchgit, reflex, reflex-dom, stdenv
, template-haskell, text
}:
mkDerivation {
  pname = "reflex-dom-helpers";
  version = "0.2.0.0";
  src = fetchgit {
    url = "https://github.com/jappeace/reflex-dom-helpers";
    sha256 = "0w3i424pp8in1271q7wp44bkxfcq7f8m3ivj5rpjk9i58zmjb2f4";
    rev = "112901c67a3c58c065469cfd45dd2d7ab8509ae6";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base reflex reflex-dom template-haskell text
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/layer-3-communications/reflex-dom-helpers";
  description = "Element tag helpers for working with reflex-dom";
  license = stdenv.lib.licenses.bsd3;
}
