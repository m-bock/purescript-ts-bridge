{ name = "sample-project"
, dependencies = [ "console", "effect", "prelude", "typescript-bridge" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
