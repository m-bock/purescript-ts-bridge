
let upstream =
  https://github.com/purescript/package-sets/releases/download/psc-0.15.8-20230517/packages.dhall
    sha256:8b94a0cd7f86589a6bd06d48cb9a61d69b66a94b668657b2f10c8b14c16e028c

in upstream

with dts =
  { repo = "https://github.com/thought2/purescript-dts.git"
  , version = "v0.2.0"
  , dependencies = [ "arrays", "console", "effect", "maybe", "newtype", "ordered-collections", "ordered-set", "prelude", "tuples" ] : List Text
  }

