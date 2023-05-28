let conf = ./spago.dhall
in conf // {
  sources = conf.sources # [ "test/**/*.purs" ],
  dependencies = conf.dependencies # 
    [ "spec-discovery"
    , "aff"
    , "spec"
    , "classless-encode-json"
    , "argonaut-core"
    , "classless"
    ]
}