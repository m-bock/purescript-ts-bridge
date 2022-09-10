module Test.Main where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import TsBridge (defaultProgOptions, printTsProgram, tsModuleFile, tsProgram, tsTypeAlias)
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] spec

spec :: Spec Unit
spec = do
  it "generates a type alias for Number" do
    tsProgram defaultProgOptions
      [ tsModuleFile "types.d.ts"
          [ tsTypeAlias "Foo" (Proxy :: _ Number) ]
      ]
      # printTsProgram
      # shouldEqual ([ "types.d.ts" /\ "type Foo = number" ])