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
  , "strings"
  , "transformers"
  , "tuples"
  , "untagged-union"
  , "variant"
  , "variant-encodings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
