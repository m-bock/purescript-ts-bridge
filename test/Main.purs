module Test.Main where

import Prelude

import Data.Maybe (Maybe)
import Data.String (joinWith)
import Data.Tuple.Nested ((/\))
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
    it "generates a type alias for Number" do
      tsProgram
        [ tsModuleFile "types.d.ts"
            [ tsTypeAlias "Foo" (Proxy :: _ Number) ]
        ]
        # printTsProgram
        # shouldEqual ([ "types.d.ts" /\ "type Foo=number" ])

    it "generates a type alias for String" do
      tsProgram
        [ tsModuleFile "types.d.ts"
            [ tsTypeAlias "Foo" (Proxy :: _ String) ]
        ]
        # printTsProgram
        # shouldEqual ([ "types.d.ts" /\ "type Foo=string" ])

    it "generates a type alias for Boolean" do
      tsProgram
        [ tsModuleFile "types.d.ts"
            [ tsTypeAlias "Foo" (Proxy :: _ Boolean) ]
        ]
        # printTsProgram
        # shouldEqual ([ "types.d.ts" /\ "type Foo=boolean" ])

    it "generates a type alias for Arrays" do
      tsProgram
        [ tsModuleFile "types.d.ts"
            [ tsTypeAlias "Foo" (Proxy :: _ (Array String)) ]
        ]
        # printTsProgram
        # shouldEqual ([ "types.d.ts" /\ "type Foo=Array<string>" ])

    it "generates a type alias for Records" do
      tsProgram
        [ tsModuleFile "types.d.ts"
            [ tsTypeAlias "Foo" (Proxy :: _ { bar :: String, foo :: Number }) ]
        ]
        # printTsProgram
        # shouldEqual ([ "types.d.ts" /\ "type Foo={bar:string;foo:number}" ])

    it "generates a type alias for Functions" do
      tsProgram
        [ tsModuleFile "types.d.ts"
            [ tsTypeAlias "Foo" (Proxy :: _ (String -> Number -> Boolean)) ]
        ]
        # printTsProgram
        # shouldEqual ([ "types.d.ts" /\ "type Foo=(_:string)=>(_:number)=>boolean" ])

  describe "Standard Types" do
    it "generates a type " do
      tsProgram
        [ tsModuleFile "types.d.ts"
            [ tsTypeAlias "Foo" (Proxy :: _ (Maybe Boolean)) ]
        ]
        # printTsProgram
        # shouldEqual
            ( [ "types.d.ts" /\
                  joinWith "\n"
                    [ "import*as Data_Maybe from 'Data.Maybe/index'"
                    , "type Foo=Data_Maybe.Maybe<boolean>"
                    ]
              , "Data.Maybe/index.d.ts" /\
                  joinWith "\n"
                    [ "type Maybe<A>={ opaque }"
                    ]
              ]
            )
