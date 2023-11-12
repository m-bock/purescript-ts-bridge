module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.TsBridgeSpec as Test.TsBridgeSpec

main :: Effect Unit
main = launchAff_ do
  runSpec [ consoleReporter ] do
    Test.TsBridgeSpec.spec