{ name = "ts-bridge-test"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "argonaut-core"
  , "arrays"
  , "classless"
  , "classless-encode-json"
  , "console"
  , "dts"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "heterogeneous"
  , "literals"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "node-fs-aff"
  , "node-path"
  , "node-process"
  , "nullable"
  , "optparse"
  , "ordered-collections"
  , "ordered-set"
  , "prelude"
  , "record"
  , "safe-coerce"
  , "spec"
  , "spec-discovery"
  , "strings"
  , "transformers"
  , "tuples"
  , "untagged-union"
  , "variant"
  , "variant-encodings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
