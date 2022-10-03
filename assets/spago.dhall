{ name = "ts-type-generation-project"
, dependencies =
      ./all-deps.dhall
    # [ "console", "effect", "either", "prelude", "typescript-bridge" ]
, packages = ../packages.dhall // ./packages.dhall
, sources = [ "src/**/*.purs", "../src/**/*.purs" ]
}
