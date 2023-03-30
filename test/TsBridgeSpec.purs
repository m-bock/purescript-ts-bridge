module Test.TsBridgeSpec
  ( spec
  ) where

import Prelude

import Data.Either (Either(..), fromRight)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.Nullable (Nullable)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple, fst)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Effect (Effect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import TsBridge as TSB
import TsBridge.Monad (TsBridgeM)
import DTS.Print (printTsDeclarations, printTsType)
import Type.Proxy (Proxy(..))
import DTS as DTS

class TsBridge (a :: Type) where
  tsBridge :: Proxy a -> TSB.StandaloneTsType

instance TsBridge Number where
  tsBridge = TSB.tsBridgeNumber

instance TsBridge String where
  tsBridge = TSB.tsBridgeString

instance TsBridge Boolean where
  tsBridge = TSB.tsBridgeBoolean

instance TsBridge a => TsBridge (Array a) where
  tsBridge = TSB.tsBridgeArray Tok

instance TsBridge a => TsBridge (Effect a) where
  tsBridge = TSB.tsBridgeEffect Tok

instance TsBridge a => TsBridge (Nullable a) where
  tsBridge = TSB.tsBridgeNullable Tok

instance (TsBridge a, TsBridge b) => TsBridge (a -> b) where
  tsBridge = TSB.tsBridgeFunction Tok

instance (TSB.TsBridgeRecord Tok r) => TsBridge (Record r) where
  tsBridge = TSB.tsBridgeRecord Tok

instance (TSB.TsBridgeVariant Tok r) => TsBridge (Variant r) where
  tsBridge = TSB.tsBridgeVariant Tok

instance TsBridge a => TsBridge (Maybe a) where
  tsBridge = TSB.tsBridgeMaybe Tok

instance (TsBridge a, TsBridge b) => TsBridge (Tuple a b) where
  tsBridge = TSB.tsBridgeTuple Tok

instance (TsBridge a, TsBridge b) => TsBridge (Either a b) where
  tsBridge = TSB.tsBridgeEither Tok

instance IsSymbol sym => TsBridge (TSB.TypeVar sym) where
  tsBridge = TSB.tsBridgeTypeVar

instance TsBridge Unit where
  tsBridge = TSB.tsBridgeUnit

newtype MyNT = MyNT Number

derive instance Newtype MyNT _

instance TsBridge MyNT where
  tsBridge = TSB.tsBridgeNewtype Tok "Foo.Bar" (TSB.DefName "MyNT") []

--

data Tok = Tok

instance TsBridge a => TSB.TsBridgeBy Tok a where
  tsBridgeBy _ = tsBridge

--

type A = TSB.TypeVar "A"
type B = TSB.TypeVar "B"
type C = TSB.TypeVar "C"
type D = TSB.TypeVar "D"

spec :: Spec Unit
spec = do
  describe "TsBridgeSpec" do
    describe "Program Printing" do
      describe "Program with imports" do
        it "generates a type and adds the type module" do
          ( TSB.tsProgram
              [ TSB.tsModuleFile "Foo.Bar"
                  [ TSB.tsValue Tok (TSB.DefName "a") (Left "" :: Either String Boolean) ]
              ]
              <#> printTsProgram
          )
            `shouldEqual`
              ( Right $ Map.fromFoldable
                  [ textFile "Foo.Bar/index.d.ts"
                      [ "export const a : import('../Data.Either').Either<string, boolean>"
                      ]
                  , textFile "Data.Either/index.d.ts"
                      [ "export type Either<A, B> = { readonly '__brand': unique symbol; readonly '__arg1': A; readonly '__arg2': B; }"
                      ]
                  ]
              )

      describe "Newtype" do
        it "generates a type and adds the type module" do
          ( TSB.tsProgram
              [ TSB.tsModuleFile "Foo.Bar"
                  [ TSB.tsValue Tok (TSB.DefName "a") (MyNT 0.0) ]
              ]
              <#> printTsProgram
          )
            `shouldEqual`
              ( Right $ Map.fromFoldable
                  [ textFile "Foo.Bar/index.d.ts"
                      [ "export const a : import('../Foo.Bar').MyNT"
                      , ""
                      , "export type MyNT = number"
                      ]
                  ]
              )

    describe "Decl tsValue" do
      describe "tsValue" do
        describe "Number" do
          testDeclPrint
            (TSB.tsValue Tok (TSB.DefName "foo") 13.0)
            [ "export const foo : number" ]

        describe "Number" do
          it "prints the correct declaration" do
            ( TSB.tsValue Tok (TSB.DefName "foo'") 0.0
                # TSB.runTsBridgeM
                <#> (fst >>> printTsDeclarations)
            )
              `shouldEqual` (Left $ TSB.ErrTsName $ TSB.ErrInvalidCharacter '\'')

        describe "Number" do
          it "prints the correct declaration" do
            ( TSB.tsValue Tok (TSB.DefName "foo") (Nothing :: Maybe A)
                # TSB.runTsBridgeM
                <#> (fst >>> printTsDeclarations)
            )
              `shouldEqual` (Left $ TSB.ErrUnquantifiedTypeVariables $ Set.fromFoldable [ TSB.unsafeName "A" ])

    describe "Type Printing" do
      describe "Number" do
        testTypePrint (tsBridge (Proxy :: _ Number))
          "number"

      describe "String" do
        testTypePrint (tsBridge (Proxy :: _ String))
          "string"

      describe "Boolean" do
        testTypePrint (tsBridge (Proxy :: _ Boolean))
          "boolean"

      describe "Array" do
        testTypePrint (tsBridge (Proxy :: _ (Array Boolean)))
          "Array<boolean>"

      describe "Effect" do
        testTypePrint (tsBridge (Proxy :: _ (Effect Unit)))
          "() => void"

      describe "Function" do
        testTypePrint (tsBridge (Proxy :: _ (String -> Number -> Boolean)))
          "(_: string) => (_: number) => boolean"

      describe "Function" do
        testTypePrint (tsBridge (Proxy :: _ (Array A -> Array B -> Array (Tuple A B))))
          "<A>(_: Array<A>) => <B>(_: Array<B>) => Array<import('../Data.Tuple').Tuple<A, B>>"

      describe "Record" do
        testTypePrint (tsBridge (Proxy :: _ { bar :: String, foo :: Number }))
          "{ readonly 'bar': string; readonly 'foo': number; }"

      describe "Maybe" do
        testTypePrint (tsBridge (Proxy :: _ (Maybe Boolean)))
          "import('../Data.Maybe').Maybe<boolean>"

      describe "Either" do
        it "prints the correct declaration" do
          ( tsBridge (Proxy :: _ (Either String Boolean))
              # TSB.runTsBridgeM
              <#> (fst >>> printTsType)
          )
            `shouldEqual` (Right $ DTS.TsSource "import('../Data.Either').Either<string, boolean>")

      describe "Nullable" do
        testTypePrint (tsBridge (Proxy :: _ (Nullable String)))
          "(null) | (string)"

      describe "Variant" do
        testTypePrint (tsBridge (Proxy :: _ (Variant (a :: String, b :: Boolean))))
          "({ readonly 'type': 'a'; readonly 'value': string; }) | ({ readonly 'type': 'b'; readonly 'value': boolean; })"

testDeclPrint :: TsBridgeM (Array DTS.TsDeclaration) -> Array String -> Spec Unit
testDeclPrint x s =
  it "prints the correct declaration" do
    TSB.runTsBridgeM x
      <#> (fst >>> printTsDeclarations)
      # fromRight []
      # shouldEqual (DTS.TsSource <$> s)

testTypePrint :: TsBridgeM DTS.TsType -> String -> Spec Unit
testTypePrint x s =
  it "prints the correct type" do
    shouldEqual
      ( TSB.runTsBridgeM x
          <#> (fst >>> printTsType)
          # fromRight (DTS.TsSource "")
      )
      (DTS.TsSource s)

textFile :: String -> Array String -> DTS.Path /\ Array String
textFile n lines = DTS.Path n /\ lines

printTsProgram :: DTS.TsProgram -> Map DTS.Path (Array String)
printTsProgram x = DTS.printTsProgram x
  <#> un DTS.TsSource >>> String.split (Pattern "\n")