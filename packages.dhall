let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20220901/packages.dhall
        sha256:f1531b29c21ac437ffe5666c1b6cc76f0a9c29d3c9d107ff047aa2567744994f

in  upstream
  with node-glob-basic =
    { dependencies =
      [ "aff"
      , "console"
      , "effect"
      , "lists"
      , "maybe"
      , "node-fs-aff"
      , "node-path"
      , "node-process"
      , "ordered-collections"
      , "strings"
      ]
    , repo = "https://github.com/natefaubion/purescript-node-glob-basic.git"
    , version = "v1.2.2"
    }
  with record-extra =
    { dependencies =
      [ "arrays", "functions", "lists", "record", "typelevel-prelude" ]
    , repo = "https://github.com/justinwoo/purescript-record-extra.git"
    , version = "0.15.0-starter-kit"
    }
  with node-workerbees =
    { dependencies =
      [ "aff"
      , "argonaut-core"
      , "arraybuffer-types"
      , "avar"
      , "effect"
      , "either"
      , "exceptions"
      , "maybe"
      , "newtype"
      , "parallel"
      , "variant"
      ]
    , repo = "https://github.com/natefaubion/purescript-node-workerbees.git"
    , version = "node-esm"
    }
  with tidy =
    { dependencies =
      [ "arrays"
      , "control"
      , "dodo-printer"
      , "either"
      , "foldable-traversable"
      , "lists"
      , "maybe"
      , "newtype"
      , "ordered-collections"
      , "partial"
      , "prelude"
      , "language-cst-parser"
      , "strings"
      , "tuples"
      ]
    , repo = "https://github.com/natefaubion/purescript-tidy.git"
    , version = "fa1c7c7b251341f3a22ca4e402f46614486878db"
    }
  with typedenv =
    { dependencies =
      [ "aff"
      , "effect"
      , "either"
      , "foldable-traversable"
      , "foreign-object"
      , "integers"
      , "maybe"
      , "numbers"
      , "prelude"
      , "record"
      , "spec"
      , "strings"
      , "type-equality"
      , "typelevel-prelude"
      ]
    , repo = "https://github.com/thought2/purescript-typedenv.git"
    , version = "purs15"
    }
  with ps-cst = ../purescript-ps-cst/spago.dhall as Location