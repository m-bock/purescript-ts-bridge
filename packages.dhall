let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.8-20230425/packages.dhall
        sha256:74bd0986f8c25c67be10cd702466425caaefe495981a49b21092cb746ca63768

in upstream

with variant-encodings =
      { dependencies =
          [ "prelude", "unsafe-coerce", "variant" ]
      , repo =
          "https://github.com/thought2/purescript-variant-encodings.git"
      , version =
          "main"
      }