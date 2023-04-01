let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.7-20230331/packages.dhall
        sha256:97a54e4c5c1a76f51cef8fb8c91a8ff602dca7828dc464e07e48ee563b6bd058

in  upstream

  with dts =
    { dependencies =
      [ "arrays"
      , "console"
      , "effect"
      , "maybe"
      , "newtype"
      , "ordered-collections"
      , "ordered-set"
      , "prelude"
      , "tuples"
      ]
    , repo = "https://github.com/thought2/purescript-dts.git"
    , version = "v0.1.3"
    }