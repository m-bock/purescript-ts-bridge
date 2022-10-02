module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.TsBridge as Test.TsBridge
import Test.TsBridgeGen as Test.TsBridgeGen


main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  Test.TsBridge.spec
  Test.TsBridgeGen.spec
  
