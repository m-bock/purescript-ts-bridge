module MyTsBridgeModules where

{-GEN:imports
{}
-}

import Prelude

import Effect (Effect)
import MyTsBridgeClass (MappingToTsBridge(..))
import TsBridge (TsProgram, mergeTsPrograms, mkTypeGenCli, tsModuleFile, tsOpaqueType, tsProgram, tsTypeAlias, tsUnsupported)
import Type.Proxy (Proxy(..))

{-GEN:END-}

finalTsProgram :: TsProgram
finalTsProgram = generatedTsProgram `mergeTsPrograms` additionalTsProgram

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

main :: Effect Unit
main = mkTypeGenCli finalTsProgram