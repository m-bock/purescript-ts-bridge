module Test.Main where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe)
import Data.String as S
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import TsBridge (printTsProgram, tsModuleFile, tsProgram, tsTypeAlias)
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] spec

spec :: Spec Unit
spec = do
  describe "Primitives" do
    describe "Number" do
      it "generates a type alias" do
        tsProgram
          [ tsModuleFile "types.d.ts"
              [ tsTypeAlias "Foo" (Proxy :: _ Number) ]
          ]
          # printTsProgram
          # shouldEqual
          $ Map.fromFoldable
              [ textFile "types.d.ts"
                  [ "type Foo = number" ]
              ]

    describe "String" do
      it "generates a type alias" do
        tsProgram
          [ tsModuleFile "types.d.ts"
              [ tsTypeAlias "Foo" (Proxy :: _ String) ]
          ]
          # printTsProgram
          # shouldEqual
          $ Map.fromFoldable
              [ textFile "types.d.ts"
                  [ "type Foo = string" ]
              ]

    describe "Boolean" do
      it "generates a type alias" do
        tsProgram
          [ tsModuleFile "types.d.ts"
              [ tsTypeAlias "Foo" (Proxy :: _ Boolean) ]
          ]
          # printTsProgram
          # shouldEqual
          $ Map.fromFoldable
              [ textFile "types.d.ts"
                  [ "type Foo = boolean" ]
              ]

    describe "Array" do
      it "generates a type alias" do
        tsProgram
          [ tsModuleFile "types.d.ts"
              [ tsTypeAlias "Foo" (Proxy :: _ (Array String)) ]
          ]
          # printTsProgram
          # shouldEqual
          $ Map.fromFoldable
              [ textFile "types.d.ts"
                  [ "type Foo = Array<string>" ]
              ]

    describe "Record" do
      it "generates a type alias" do
        tsProgram
          [ tsModuleFile "types.d.ts"
              [ tsTypeAlias "Foo" (Proxy :: _ { bar :: String, foo :: Number }) ]
          ]
          # printTsProgram
          # shouldEqual
          $ Map.fromFoldable
              [ textFile "types.d.ts"
                  [ "type Foo = { bar: string; foo: number; }" ]
              ]

    describe "Function" do
      it "generates a type alias" do
        tsProgram
          [ tsModuleFile "types.d.ts"
              [ tsTypeAlias "Foo" (Proxy :: _ (String -> Number -> Boolean)) ]
          ]
          # printTsProgram
          # shouldEqual
          $ Map.fromFoldable
              [ textFile "types.d.ts"
                  [ "type Foo = (_: string) => (_: number) => boolean" ]
              ]

--   describe "Standard Types" do
--     it "generates a type " do
--       tsProgram
--         [ tsModuleFile "types.d.ts"
--             [ tsTypeAlias "Foo" (Proxy :: _ (Maybe Boolean)) ]
--         ]
--         # printTsProgram
--         # shouldEqual
--         $ Map.fromFoldable
--             [ textFile "types.d.ts"
--                 [ "import*as Data_Maybe from 'Data.Maybe/index'"
--                 , "type Foo=Data_Maybe.Maybe<boolean>"
--                 ]
--             , textFile "Data.Maybe/index.d.ts"
--                 [ "type Maybe<A>={ opaque }"
--                 ]
--             ]

textFile :: String -> Array String -> String /\ String
textFile n lines = n /\ S.joinWith "\n" lines