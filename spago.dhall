{ name = "ts-bridge"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "arrays"
  , "console"
  , "dts"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign"
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
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
