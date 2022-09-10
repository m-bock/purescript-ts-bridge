module Main where

import Prelude

import Effect (Effect)
import TsBridge (TsProgram, tsModuleFile, tsProgram, tsTypeAlias)
import TsBridge.Cli (mkTypeGenCli)
import Type.Proxy (Proxy(..))

type User = { name :: String, hobbies :: Array String }

myTsProgram :: TsProgram
myTsProgram =
  tsProgram
    [ tsModuleFile "types.d.ts"
        [ tsTypeAlias "Foo" (Proxy :: _ Number) ]
    ]

main :: Effect Unit
main = mkTypeGenCli myTsProgram
