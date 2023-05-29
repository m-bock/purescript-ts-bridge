
let upstream =
  https://github.com/purescript/package-sets/releases/download/psc-0.15.8-20230517/packages.dhall
    sha256:8b94a0cd7f86589a6bd06d48cb9a61d69b66a94b668657b2f10c8b14c16e028c

in upstream


with virtual-dom =
  { repo = "https://github.com/thought2/purescript-virtual-dom.git"
  , version = "2e9a410718444e2adfb62b05f6144a1fca0d20f5"
  , dependencies = [ "either", "foreign", "maybe", "prelude", "strings", "transformers", "tuples", "variant" ]
  }


with virtual-dom-react-basic =
  { repo = "https://github.com/thought2/purescript-virtual-dom-react-basic.git"
  , version = "2efb8a3bb000fe53cfc76ebb770f5ffed0565e1d"
  , dependencies = [ "console", "effect", "foreign", "foreign-object", "maybe", "prelude", "react-basic", "react-basic-dom", "strings", "tuples", "unsafe-coerce", "virtual-dom" ]
  }


with data-mvc =
  { repo = "https://github.com/thought2/purescript-data-mvc.git"
  , version = "1a8d7e1ab72feaa622e2ef0fd84facfb0bba9f44"
  , dependencies = [ "heterogeneous", "maybe", "newtype", "prelude", "record", "variant" ]
  }


with virtual-dom-halogen =
  { repo = "https://github.com/thought2/purescript-virtual-dom-halogen.git"
  , version = "e8ac7a6a67f4fd50ddb62c6c0bf37070803843a5"
  , dependencies = [ "aff", "arrays", "bifunctors", "effect", "foreign", "halogen", "prelude", "safe-coerce", "strings", "tuples", "unsafe-coerce", "virtual-dom", "web-events", "web-html" ]
  }


with marked =
  { repo = "ssh://git@github.com/thought2/purescript-marked.git"
  , version = "e9505e2438fdf5dd975fcac96a759189b9574563"
  , dependencies = [ "console", "dts", "effect", "either", "integers", "labeled-data", "maybe", "newtype", "nullable", "prelude", "ts-bridge", "unsafe-coerce", "untagged-union", "variant", "variant-encodings" ]
  }


with variant-encodings =
  { repo = "https://github.com/thought2/purescript-variant-encodings.git"
  , version = "a4ad3a69e3f1fa6a50d511ae9035ca49c04f36ae"
  , dependencies = [ "prelude", "unsafe-coerce", "variant" ]
  }


with ts-bridge =
  { repo = "https://github.com/thought2/purescript-ts-bridge.git"
  , version = "b6d620ad06889ec1fcf002a488577d10e7f0698d"
  , dependencies = [ "aff", "aff-promise", "arrays", "console", "dts", "effect", "either", "foldable-traversable", "literals", "maybe", "newtype", "node-buffer", "node-fs", "node-fs-aff", "node-path", "node-process", "nullable", "optparse", "ordered-collections", "ordered-set", "partial", "prelude", "record", "safe-coerce", "strings", "transformers", "tuples", "typelevel-prelude", "unsafe-coerce", "untagged-union", "variant", "variant-encodings" ]
  }


with dts =
  { repo = "https://github.com/thought2/purescript-dts.git"
  , version = "62184ea266bfe6a980471e3e3bf64ff57b19fa7c"
  , dependencies = [ "arrays", "console", "effect", "maybe", "newtype", "ordered-collections", "ordered-set", "prelude", "tuples" ]
  }


with labeled-data =
  { repo = "https://github.com/thought2/purescript-labeled-data.git"
  , version = "fde21ec576f265a777f7a78fa1a8f5f582696641"
  , dependencies = [ "aff", "effect", "either", "maybe", "prelude", "record", "tuples", "type-equality", "unsafe-coerce", "variant" ]
  }

