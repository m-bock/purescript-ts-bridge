module MyTsBridgeModules where

{-GEN:imports
{}
-}

import Prelude

import Effect (Effect)
import MyTsBridgeClass (MappingToTsBridge(..))
import Prelude
import TsBridge as TSB
import Type.Proxy (Proxy(..))

{-GEN:END-}

finalTsProgram :: TSB.TsProgram
finalTsProgram = generatedTsProgram `TSB.mergeTsPrograms` additionalTsProgram

additionalTsProgram :: TSB.TsProgram
additionalTsProgram =
  TSB.tsProgram
    [ TSB.tsModuleFile "SampleApp.Types/index"
        [
        ]

    ]

{-GEN:ts-program
{ "include": ["SampleApp.**.*"], "exclude": [] }
-}

generatedTsProgram :: TSB.TsProgram
generatedTsProgram = TSB.tsProgram []

{-GEN:END-}

main :: Effect Unit
main = TSB.mkTypeGenCli finalTsProgram