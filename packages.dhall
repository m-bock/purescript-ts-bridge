let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.10-20230801/packages.dhall
        sha256:60046d79b997d4f4b18e934904ce33dbf1be24852cef43121848941e3b9e7f6c

in  upstream
  with dts =
    { repo = "https://github.com/thought2/purescript-dts.git"
    , version = "v0.2.0"
    , dependencies =
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
        : List Text
    }
