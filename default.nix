{ mkDerivation
, aeson
, base64-bytestring
, bifunctors
, data-default
, ghc
, ghcjs-base ? null
, ghcjs-dom
, http-types
, lens
, mtl
, random
, readable
, reflex
, reflex-dom
, safe
, semigroups
, string-conv
, text
, these
, time
, transformers
, uri-bytestring
, webkitgtk3-javascriptcore
}:

mkDerivation {
  pname = "reflex-dom-contrib";
  version = "0.5";
  src = builtins.filterSource (path: type: baseNameOf path != ".git") ./.;
  buildDepends = [
    aeson
    base64-bytestring
    bifunctors
    data-default
    ghcjs-base
    ghcjs-dom
    http-types
    lens
    mtl
    random
    readable
    reflex
    reflex-dom
    safe
    semigroups
    string-conv
    text
    these
    time
    transformers
    uri-bytestring
  ];
  license = null;
}
