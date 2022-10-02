module MyTsBridgeModules where

import SampleApp.Types (AppState)
import TsBridge (TsProgram, tsModuleFile, tsProgram, tsTypeAlias)
import Type.Proxy (Proxy(..))

{-GEN:imports-}

{-GEN:END-}

tsProgram :: TsProgram
tsProgram = generatedTsProgram `mergeTsPrograms` additionalTsProgram

additionalTsProgram :: TsProgram
additionalTsProgram =
  tsProgram
    [ tsModuleFile "SampleApp.Types/index"
        [ -- tsTypeAlias "Bar" (Proxy :: _ (AppState))
        ]

    ]

{-GEN:ts-program
{ "include": [ "**" ]
, "exclude": []
}
-}

generatedTsProgram :: TsProgram
generatedTsProgram = tsProgram []

{-GEN:END-}