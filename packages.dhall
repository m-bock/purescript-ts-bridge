let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20220901/packages.dhall
        sha256:f1531b29c21ac437ffe5666c1b6cc76f0a9c29d3c9d107ff047aa2567744994f

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
    , version = "ea900ff5108f6dec7c7fbab14cb6c09d036c0d63"
    }
  