module Test.Main where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Argonaut as J
import Data.Either (Either)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Error, launchAff_)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import TsBridge (TsProgram, tsModuleFile, tsProgram, tsTypeAlias)
import TsBridge as TsBridge
import TsBridge.ABC (A, B, C)
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] spec

spec :: Spec Unit
spec = do
  describe "Primitives" do
    describe "Number" do
      it "generates a type alias" do
        tsProgram
          [ tsModuleFile "types"
              [ tsTypeAlias "Foo" (Proxy :: _ Number) ]
          ]
          # printTsProgram
          # shouldEqual
          $ Map.fromFoldable
              [ textFile "types.d.ts"
                  [ "export type Foo = number" ]
              ]

    describe "String" do
      it "generates a type alias" do
        tsProgram
          [ tsModuleFile "types"
              [ tsTypeAlias "Foo" (Proxy :: _ String) ]
          ]
          # printTsProgram
          # shouldEqual
          $ Map.fromFoldable
              [ textFile "types.d.ts"
                  [ "export type Foo = string" ]
              ]

    describe "Boolean" do
      it "generates a type alias" do
        tsProgram
          [ tsModuleFile "types"
              [ tsTypeAlias "Foo" (Proxy :: _ Boolean) ]
          ]
          # printTsProgram
          # shouldEqual
          $ Map.fromFoldable
              [ textFile "types.d.ts"
                  [ "export type Foo = boolean" ]
              ]

    describe "Array" do
      it "generates a type alias" do
        tsProgram
          [ tsModuleFile "types"
              [ tsTypeAlias "Foo" (Proxy :: _ (Array String)) ]
          ]
          # printTsProgram
          # shouldEqual
          $ Map.fromFoldable
              [ textFile "types.d.ts"
                  [ "export type Foo = Array<string>" ]
              ]

    describe "Record" do
      it "generates a type alias" do
        tsProgram
          [ tsModuleFile "types"
              [ tsTypeAlias "Foo" (Proxy :: _ { bar :: String, foo :: Number }) ]
          ]
          # printTsProgram
          # shouldEqual
          $ Map.fromFoldable
              [ textFile "types.d.ts"
                  [ "export type Foo = { readonly bar: string; readonly foo: number; }" ]
              ]

    describe "Function" do
      it "generates a type alias" do
        tsProgram
          [ tsModuleFile "types"
              [ tsTypeAlias "Foo" (Proxy :: _ (String -> Number -> Boolean)) ]
          ]
          # printTsProgram
          # shouldEqual
          $ Map.fromFoldable
              [ textFile "types.d.ts"
                  [ "export type Foo = (_: string) => (_: number) => boolean" ]
              ]

    describe "Standard Types" do
      describe "Maybe" do
        it "generates a type alias and adds the type module" do
          tsProgram
            [ tsModuleFile "types"
                [ tsTypeAlias "Foo" (Proxy :: _ (Maybe Boolean)) ]
            ]
            # printTsProgram
            # shouldEqual
            $ Map.fromFoldable
                [ textFile "types.d.ts"
                    [ "import * as Data_Maybe from 'Data.Maybe/index'"
                    , ""
                    , "export type Foo = Data_Maybe.Maybe<boolean>"
                    ]
                , textFile "Data.Maybe/index.d.ts"
                    [ "export type Maybe<A> = { readonly opaque_Maybe: unique symbol; readonly arg0: A; }"
                    ]
                ]

      describe "Either" do
        it "generates a type alias and adds the type module" do
          tsProgram
            [ tsModuleFile "types"
                [ tsTypeAlias "Foo" (Proxy :: _ (Either String Boolean)) ]
            ]
            # printTsProgram
            # shouldEqual
            $ Map.fromFoldable
                [ textFile "types.d.ts"
                    [ "import * as Data_Either from 'Data.Either/index'"
                    , ""
                    , "export type Foo = Data_Either.Either<string, boolean>"
                    ]
                , textFile "Data.Either/index.d.ts"
                    [ "export type Either<A, B> = { readonly opaque_Either: unique symbol; readonly arg0: A; readonly arg1: B; }"
                    ]
                ]

    describe "Type Variables" do
      describe "Single" do
        it "generates a type alias with a quantified type variable" do
          tsProgram
            [ tsModuleFile "types"
                [ tsTypeAlias "Foo" (Proxy :: _ A) ]
            ]
            # printTsProgram
            # shouldEqual
            $ Map.fromFoldable
                [ textFile "types.d.ts"
                    [ "export type Foo<A> = A"
                    ]
                ]

      describe "Multiple" do
        it "generates a type alias with quantified type variables" do
          tsProgram
            [ tsModuleFile "types"
                [ tsTypeAlias "Foo" (Proxy :: _ { c :: C, sub :: { a :: A, b :: B } }) ]
            ]
            # printTsProgram
            # shouldEqual
            $ Map.fromFoldable
                [ textFile "types.d.ts"
                    [ "export type Foo<C, A, B> = { readonly c: C; readonly sub: { readonly a: A; readonly b: B; }; }"
                    ]
                ]

      describe "Function" do
        describe "A" do
          it "generates a type alias with quantified type variables" do
            tsProgram
              [ tsModuleFile "types"
                  [ tsTypeAlias "Foo" (Proxy :: _ (A -> B -> C)) ]
              ]
              # printTsProgram
              # shouldEqual
              $ Map.fromFoldable
                  [ textFile "types.d.ts"
                      [ "export type Foo = <A>(_: A) => <B, C>(_: B) => C"
                      ]
                  ]

        describe "B" do
          it "generates a type alias with quantified type variables" do
            tsProgram
              [ tsModuleFile "types"
                  [ tsTypeAlias "Foo" (Proxy :: _ (A -> A -> A)) ]
              ]
              # printTsProgram
              # shouldEqual
              $ Map.fromFoldable
                  [ textFile "types.d.ts"
                      [ "export type Foo = <A>(_: A) => (_: A) => A"
                      ]
                  ]

textFile :: String -> Array String -> String /\ Array String
textFile n lines = n /\ lines

shouldEqual :: forall m t. MonadThrow Error m => EncodeJson t => Show t => Eq t => t -> t -> m Unit
shouldEqual v1 v2 = when (v1 /= v2)
  $ fail
  $ show v1 <> " ≠ " <> show v2
      <> "\n\n"
      <> "JSON:"
      <> "\n\n"
      <> showJson v1
      <> "\n\n≠\n\n"
      <> showJson v2
      <> "\n"
  where
  showJson = encodeJson >>> J.stringifyWithIndent 2

printTsProgram :: TsProgram -> Map String (Array String)
printTsProgram x = TsBridge.printTsProgram x <#> String.split (Pattern "\n")