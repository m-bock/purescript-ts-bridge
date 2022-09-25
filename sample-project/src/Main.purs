module Main where

import Prelude

import Effect (Effect)
import SampleApp.Types (AppState)
import TsBridge (TsProgram, tsModuleFile, tsProgram, tsTypeAlias)
import TsBridge.Cli (mkTypeGenCli)
import Type.Proxy (Proxy(..))

type User = { name :: String, hobbies :: Array String }


myTsProgram :: TsProgram
myTsProgram =
  tsProgram
    [ tsModuleFile "Main/index"
        [ tsTypeAlias "Foo" (Proxy :: _ Number)
        , tsTypeAlias "Bar" (Proxy :: _ (Number -> String -> AppState))
        ]
    ]

main :: Effect Unit
main = mkTypeGenCli myTsProgram
