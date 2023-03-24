module Test.Main where

import Prelude

import Data.String as Str
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Discovery (discover)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ do
  specs <- discover $ Str.joinWith "|"
    [ """(Test.TsBridge.*Spec)"""
    ]
  runSpec [ consoleReporter ] specs