module TsBridgeSpec
  ( spec
  ) where

import Prelude

import Data.Either (Either)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.String (Pattern(..))
import Data.String as String
import Data.Symbol (class IsSymbol)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Effect (Effect)
import Test.Spec (Spec, describe, it)
import Test.Util (shouldEqual)
import TsBridge (class DefaultRecord, class DefaultVariant, class TsBridgeBy, TsDeclaration, TsProgram, TsType, Var(..), runTsBridgeM, tsValue)
import TsBridge as TSB
import TsBridge.Monad (TsBridgeM)
import TsBridge.Print (printTsDeclarations, printTsType)
import Type.Proxy (Proxy(..))

class TsBridge a where
  tsBridge :: a -> TsBridgeM TsType

instance TsBridge a => TsBridge (Proxy a) where
  tsBridge = TSB.defaultProxy Tok

instance TsBridge Number where
  tsBridge = TSB.defaultNumber

instance TsBridge String where
  tsBridge = TSB.defaultString

instance TsBridge Boolean where
  tsBridge = TSB.defaultBoolean

instance TsBridge a => TsBridge (Array a) where
  tsBridge = TSB.defaultArray Tok

instance TsBridge a => TsBridge (Effect a) where
  tsBridge = TSB.defaultEffect Tok

instance TsBridge a => TsBridge (Nullable a) where
  tsBridge = TSB.defaultNullable Tok

instance (TsBridge a, TsBridge b) => TsBridge (a -> b) where
  tsBridge = TSB.defaultFunction Tok

instance (DefaultRecord Tok r) => TsBridge (Record r) where
  tsBridge = TSB.defaultRecord Tok


instance (DefaultVariant Tok r) => TsBridge (Variant r) where
  tsBridge = TSB.defaultVariant Tok

instance TsBridge a => TsBridge (Maybe a) where
  tsBridge = TSB.defaultOpaqueType "Data.Maybe" "Maybe" [ "A" ]
    [ tsBridge (Proxy :: _ a) ]

instance (TsBridge a, TsBridge b) => TsBridge (Either a b) where
  tsBridge = TSB.defaultOpaqueType "Data.Either" "Either" [ "A", "B" ]
    [ tsBridge (Proxy :: _ a), tsBridge (Proxy :: _ b) ]

instance IsSymbol sym => TsBridge (Var sym) where
  tsBridge _ = TSB.defaultTypeVar (Var :: _ sym)

instance TsBridge Unit where
  tsBridge = TSB.defaultUnit

--

data Tok = Tok

instance TsBridge a => TsBridgeBy Tok a where
  tsBridgeBy _ = tsBridge

--

spec :: Spec Unit
spec = do
  describe "TsBridgeSpec" do
    -- describe "Program Printing" do
    -- describe "Program with imports" do
    --   it "generates a type alias and adds the type module" do
    --     tsProgram
    --       [ tsModuleFile "types"
    --           [ tsTypeAlias Tok "Foo" (Proxy :: _ (Either String Boolean)) ]
    --       , tsModuleFile "Data.Either/index"
    --           [ tsOpaqueType Tok "Either" (Proxy :: _ (Either String Boolean)) ]
    --       ]
    --       # printTsProgram
    --       # shouldEqual
    --       $ Map.fromFoldable
    --           [ textFile "types.d.ts"
    --               [ "import * as Data_Either from '~/Data.Either/index'"
    --               , ""
    --               , "export type Foo = Data_Either.Either<string, boolean>"
    --               ]
    --           , textFile "Data.Either/index.d.ts"
    --               [ "import * as Data_Either from '~/Data.Either/index'"
    --               , ""
    --               , "export type Either<A, B> = { readonly opaque_Either: unique symbol; readonly arg0: A; readonly arg1: B; }"
    --               ]
    --           ]

    describe "Declaration Printing" do
      -- describe "tsTypeAlias" do
      --   describe "Number" do
      --     testDeclPrint
      --       (tsTypeAlias Tok "Foo" (Proxy :: _ Number))
      --       [ "export type Foo = number" ]

      --   describe "Type Variable" do
      --     testDeclPrint
      --       (tsTypeAlias Tok "Foo" (Proxy :: _ A))
      --       [ "export type Foo<A> = A" ]

      --   describe "Type Variables" do
      --     testDeclPrint
      --       (tsTypeAlias Tok "Foo" (Proxy :: _ { c :: C, sub :: { a :: A, b :: B } }))
      --       [ "export type Foo<C, A, B> = { readonly c: C; readonly sub: { readonly a: A; readonly b: B; }; }" ]

      --   describe "" do
      --     testDeclPrint
      --       (tsTypeAlias Tok "Foo" (Proxy :: _ (A -> B -> C)))
      --       [ "export type Foo = <A>(_: A) => <B, C>(_: B) => C" ]

      --   describe "" do
      --     testDeclPrint
      --       (tsTypeAlias Tok "Foo" (Proxy :: _ (A -> A -> A)))
      --       [ "export type Foo = <A>(_: A) => (_: A) => A" ]

      describe "tsValue" do
        describe "Number" do
          testDeclPrint
            (tsValue Tok "foo" 13.0)
            [ "export const foo : number" ]

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

      describe "Record" do
        testTypePrint (tsBridge (Proxy :: _ { bar :: String, foo :: Number }))
          "{ readonly bar: string; readonly foo: number; }"

      describe "Maybe" do
        testTypePrint (tsBridge (Proxy :: _ (Maybe Boolean)))
          "import('~/Data.Maybe').Maybe<boolean>"

      describe "Either" do
        testTypePrint (tsBridge (Proxy :: _ (Either String Boolean)))
          "import('~/Data.Either').Either<string, boolean>"

      describe "Nullable" do
        testTypePrint (tsBridge (Proxy :: _ (Nullable String)))
          "(null)|(string)"

      describe "Variant" do
        testTypePrint (tsBridge (Proxy :: _ (Variant (a:: String, b :: Boolean))))
          "({ readonly type: 'a'; readonly value: string; })|({ readonly type: 'b'; readonly value: boolean; })"


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
    shouldEqual
      ( runTsBridgeM x
          # fst
          # printTsType
      )
      s

textFile :: String -> Array String -> String /\ Array String
textFile n lines = n /\ lines

printTsProgram :: TsProgram -> Map String (Array String)
printTsProgram x = TSB.printTsProgram x <#> String.split (Pattern "\n")