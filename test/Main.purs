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
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Error, launchAff_)
import Heterogeneous.Mapping (class Mapping)
import Prim.RowList (class RowToList)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import TsBridge (class GenRecord, TsDeclaration, TsProgram, TsType, runTsBridgeM, tsModuleFile, tsProgram, tsTypeAlias, tsValue)
import TsBridge as TsBridge
import TsBridge.ABC (A, B, C)
import TsBridge.Class (defaultArray, defaultBoolean, defaultFunction, defaultNumber, defaultProxy, defaultRecord, defaultString, tsOpaqueType1, tsOpaqueType2, tsTypeVar)
import TsBridge.Monad (TsBridgeM)
import TsBridge.Print (printTsDeclarations, printTsType)
import Type.Proxy (Proxy(..))

class ToTsBridge a where
  toTsBridge :: a -> TsBridgeM TsType

instance ToTsBridge a => ToTsBridge (Proxy a) where
  toTsBridge = defaultProxy MappingToTsBridge

instance ToTsBridge Number where
  toTsBridge = defaultNumber

instance ToTsBridge String where
  toTsBridge = defaultString

instance ToTsBridge Boolean where
  toTsBridge = defaultBoolean

instance ToTsBridge a => ToTsBridge (Array a) where
  toTsBridge = defaultArray MappingToTsBridge

instance (ToTsBridge a, ToTsBridge b) => ToTsBridge (a -> b) where
  toTsBridge = defaultFunction MappingToTsBridge

instance (GenRecord MappingToTsBridge rl, RowToList r rl) => ToTsBridge (Record r) where
  toTsBridge = defaultRecord MappingToTsBridge

instance ToTsBridge a => ToTsBridge (Maybe a) where
  toTsBridge = tsOpaqueType1 MappingToTsBridge "Data.Maybe" "Maybe" "A"

instance (ToTsBridge a, ToTsBridge b) => ToTsBridge (Either a b) where
  toTsBridge = tsOpaqueType2 MappingToTsBridge "Data.Either" "Either" "A" "B"

instance ToTsBridge A where
  toTsBridge _ = tsTypeVar "A"

instance ToTsBridge B where
  toTsBridge _ = tsTypeVar "B"

instance ToTsBridge C where
  toTsBridge _ = tsTypeVar "C"

--

data MappingToTsBridge = MappingToTsBridge

instance ToTsBridge a => Mapping MappingToTsBridge a (TsBridgeM TsType) where
  mapping _ = toTsBridge

--

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] spec

spec :: Spec Unit
spec = do
  describe "Program Printing" spec_programPrinting

  describe "Declaration Printing" do
    describe "tsTypeAlias" spec_dp_tsTypeAlias

    describe "tsValue" spec_dp_tsValue

  describe "Type Printing" spec_typePrinting

spec_dp_tsTypeAlias :: Spec Unit
spec_dp_tsTypeAlias = do
  describe "Number" do
    testDeclPrint
      (tsTypeAlias "Foo" $ toTsBridge (Proxy :: _ Number))
      [ "export type Foo = number" ]

  describe "Type Variable" do
    testDeclPrint
      (tsTypeAlias "Foo" $ toTsBridge (Proxy :: _ A))
      [ "export type Foo<A> = A" ]

  describe "Type Variables" do
    testDeclPrint
      (tsTypeAlias "Foo" $ toTsBridge (Proxy :: _ { c :: C, sub :: { a :: A, b :: B } }))
      [ "export type Foo<C, A, B> = { readonly c: C; readonly sub: { readonly a: A; readonly b: B; }; }" ]

  describe "" do
    testDeclPrint
      (tsTypeAlias "Foo" $ toTsBridge (Proxy :: _ (A -> B -> C)))
      [ "export type Foo = <A>(_: A) => <B, C>(_: B) => C" ]

  describe "" do
    testDeclPrint
      (tsTypeAlias "Foo" $ toTsBridge (Proxy :: _ (A -> A -> A)))
      [ "export type Foo = <A>(_: A) => (_: A) => A" ]

spec_dp_tsValue :: Spec Unit
spec_dp_tsValue = do
  describe "Number" do
    testDeclPrint
      (tsValue "foo" $ toTsBridge 13.0)
      [ "export const foo : number" ]

spec_typePrinting :: Spec Unit
spec_typePrinting = do
  describe "Number" do
    testTypePrint (toTsBridge (Proxy :: _ Number))
      "number"

  describe "String" do
    testTypePrint (toTsBridge (Proxy :: _ String))
      "string"

  describe "Boolean" do
    testTypePrint (toTsBridge (Proxy :: _ Boolean))
      "boolean"

  describe "Array" do
    testTypePrint (toTsBridge (Proxy :: _ (Array Boolean)))
      "Array<boolean>"

  describe "Function" do
    testTypePrint (toTsBridge (Proxy :: _ (String -> Number -> Boolean)))
      "(_: string) => (_: number) => boolean"

  describe "Record" do
    testTypePrint (toTsBridge (Proxy :: _ { bar :: String, foo :: Number }))
      "{ readonly bar: string; readonly foo: number; }"

  describe "Maybe" do
    testTypePrint (toTsBridge (Proxy :: _ (Maybe Boolean)))
      "Data_Maybe.Maybe<boolean>"

  describe "Either" do
    testTypePrint (toTsBridge (Proxy :: _ (Either String Boolean)))
      "Data_Either.Either<string, boolean>"

spec_programPrinting :: Spec Unit
spec_programPrinting = do
  describe "Program with imports" do
    it "generates a type alias and adds the type module" do
      tsProgram
        [ tsModuleFile "types"
            [ tsTypeAlias "Foo" $ toTsBridge (Proxy :: _ (Either String Boolean)) ]
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

testDeclPrint :: TsBridgeM (Array TsDeclaration) -> Array String -> Spec Unit
testDeclPrint x s =
  it "prints the correct declaration" do
    runTsBridgeM x
      # fst
      # printTsDeclarations
      # shouldEqual s

testTypePrint :: TsBridgeM TsType -> String -> Spec Unit
testTypePrint x s =
  it "prints the correct type" do
    runTsBridgeM x
      # fst
      # printTsType
      # shouldEqual s

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