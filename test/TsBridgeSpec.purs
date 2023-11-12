module Test.TsBridgeSpec
  ( spec
  ) where

import Prelude

import DTS as DTS
import DTS.Print (printTsDeclarations, printTsType)
import Data.Either (Either(..), fromRight)
import Data.Foldable (fold)
import Data.Function.Uncurried (Fn2, Fn3, Fn4)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.Nullable (Nullable)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple, fst)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Data.Variant as V
import Data.Variant.Encodings.Flat (VariantEncodedFlat)
import Data.Variant.Encodings.Nested (VariantEncodedNested)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4)
import Literals (StringLit)
import Literals.Undefined as Lit
import Prim.Boolean (False, True)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Util as U
import TsBridge (class TsBridge, tsBridge)
import TsBridge as TSB
import TsBridge.Monad (TsBridgeM)
import TsBridge.Types.TsRecord (Mod, TsRecord)
import Type.Proxy (Proxy(..))
import Untagged.Union (type (|+|))

newtype MyNT = MyNT Number

derive instance Newtype MyNT _

instance TsBridge MyNT where
  tsBridge =
    TSB.tsBridgeNewtype TSB.Tok
      { moduleName: "Foo.Bar", typeName: "MyNT", typeArgs: [] }

--

newtype RecListStr = RecListStr
  ( Variant
      ( cons :: { head :: String, tail :: RecListStr }
      , nil :: {}
      )
  )

derive instance Newtype RecListStr _

instance TsBridge RecListStr where
  tsBridge x =
    TSB.tsBridgeNewtype TSB.Tok
      { moduleName: "Data.RecListStr", typeName: "RecListStr", typeArgs: [] }
      x

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
                  [ TSB.tsValue TSB.Tok "a" (Left "" :: Either String Boolean) ]
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

      describe "Duplicate Identifiers" do
        it "does not allow duplicate identifiers in a module" do
          ( TSB.tsProgram
              [ TSB.tsModuleFile "Foo.Bar"
                  [ TSB.tsValue TSB.Tok "a" (MyNT 0.0)
                  , TSB.tsValue TSB.Tok "a" (MyNT 0.0)
                  ]
              ]
              <#> printTsProgram
          )
            `shouldEqual`
              (Left $ TSB.AtModule "Foo.Bar" $ TSB.ErrDuplicateIdentifier $ DTS.TsName "a")

      describe "Reserved words" do
        it "does not allow identifiers to be reserved words" do
          ( TSB.tsProgram
              [ TSB.tsModuleFile "Foo.Bar"
                  [ TSB.tsValues TSB.Tok { const: "" }
                  ]
              ]
              <#> printTsProgram
          )
            `shouldEqual`
              (Left $ TSB.AtModule "Foo.Bar" $ TSB.AtValue "const" $ TSB.ErrTsName $ TSB.ErrReserveredWord "const")

    describe "Decl tsValue" do
      describe "tsValue" do
        describe "Number" do
          testDeclPrint
            (TSB.tsValue TSB.Tok "foo" 13.0)
            [ "export const foo : number" ]

        describe "Number" do
          it "prints the correct declaration" do
            ( TSB.tsValue TSB.Tok "foo'" 0.0
                # TSB.runTsBridgeM
                <#> (fst >>> printTsDeclarations)
            )
              `shouldEqual` (Left $ TSB.AtValue "foo'" $ TSB.ErrTsName $ TSB.ErrInvalidCharacter '\'')

        describe "Number" do
          it "prints the correct declaration" do
            ( TSB.tsValue TSB.Tok "foo" (Nothing :: Maybe A)
                # TSB.runTsBridgeM
                <#> (fst >>> printTsDeclarations)
            )
              `shouldEqual` (Left $ TSB.AtValue "foo" $ TSB.ErrUnquantifiedTypeVariables $ Set.fromFoldable [ DTS.TsName "A" ])

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

      describe "Fn2" do
        testTypePrint (tsBridge (Proxy :: _ (Fn2 String Number Boolean)))
          "(arg1: string, arg2: number) => boolean"

      describe "Fn3" do
        testTypePrint (tsBridge (Proxy :: _ (Fn3 String Number Number Boolean)))
          "(arg1: string, arg2: number, arg3: number) => boolean"

      describe "Fn4" do
        testTypePrint (tsBridge (Proxy :: _ (Fn4 String Number Number String Boolean)))
          "(arg1: string, arg2: number, arg3: number, arg4: string) => boolean"

      describe "EffectFn1" do
        testTypePrint (tsBridge (Proxy :: _ (EffectFn1 String Boolean)))
          "(_: string) => boolean"

      describe "EffectFn2" do
        testTypePrint (tsBridge (Proxy :: _ (EffectFn2 String Number Boolean)))
          "(arg1: string, arg2: number) => boolean"

      describe "EffectFn3" do
        testTypePrint (tsBridge (Proxy :: _ (EffectFn3 String Number Number Boolean)))
          "(arg1: string, arg2: number, arg3: number) => boolean"

      describe "EffectFn4" do
        testTypePrint (tsBridge (Proxy :: _ (EffectFn4 String Number Number String Boolean)))
          "(arg1: string, arg2: number, arg3: number, arg4: string) => boolean"

      describe "Function" do
        testTypePrint (tsBridge (Proxy :: _ (Array A -> Array B -> Array (Tuple A B))))
          "<A>(_: Array<A>) => <B>(_: Array<B>) => Array<import('../Data.Tuple').Tuple<A, B>>"

      describe "Function" do
        it "Scope is dropped" do
          ( TSB.tsProgram
              [ TSB.tsModuleFile "Foo.Bar"
                  [ TSB.tsValue TSB.Tok "foo" ((\_ _ -> unit) :: A -> B -> Unit)
                  , TSB.tsValue TSB.Tok "bar" ((\_ _ -> unit) :: C -> D -> Unit)
                  ]
              ]
              <#> printTsProgram
          )
            `shouldEqual`
              ( Right $ Map.fromFoldable
                  [ textFile "Foo.Bar/index.d.ts"
                      [ "export const foo : <A>(_: A) => <B>(_: B) => void"
                      , ""
                      , "export const bar : <C>(_: C) => <D>(_: D) => void"
                      ]
                  ]
              )

      describe "Record" do
        testTypePrint (tsBridge (Proxy :: _ { bar :: String, foo :: Number }))
          "{ readonly 'bar': string; readonly 'foo': number; }"

        testTypePrint (tsBridge (Proxy :: _ {}))
          "Record<string, never>"

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

      describe "Lit.Undefined" do
        testTypePrint (tsBridge (Proxy :: _ Lit.Undefined))
          "undefined"

      describe "StringLit" do
        testTypePrint (tsBridge (Proxy :: _ (StringLit "abc")))
          "'abc'"

      describe "OneOf" do
        testTypePrint (tsBridge (Proxy :: _ (String |+| Boolean |+| Array String)))
          "(string) | ((boolean) | (Array<string>))"

      describe "Variant" do
        testTypePrint' (tsBridge (Proxy :: _ (Variant (a :: String, b :: Boolean))))
          "({ readonly 'type': 'a'; readonly 'value': string; }) | ({ readonly 'type': 'b'; readonly 'value': boolean; })"

      describe "VariantEncodedFlat" do
        testTypePrint' (tsBridge (Proxy :: _ (VariantEncodedFlat "kind" (a :: { x :: Number }, b :: { y :: String }))))
          "(({ readonly 'kind': 'a'; })&({ readonly 'x': number; })) | (({ readonly 'kind': 'b'; })&({ readonly 'y': string; }))"

      describe "VariantEncodedNested" do
        testTypePrint' (tsBridge (Proxy :: _ (VariantEncodedNested "kind" "payload" (a :: Number, b :: String))))
          "({ readonly 'kind': 'a'; readonly 'payload': number; }) | ({ readonly 'kind': 'b'; readonly 'payload': string; })"

      describe "Recursive type" do
        it "should handle recursive types without stack overflow" do
          shouldEqual
            ( TSB.tsProgram
                [ TSB.tsModuleFile "Foo.Bar"
                    [ TSB.tsValue TSB.Tok "someList"
                        ( RecListStr $ V.inj (Proxy :: _ "cons")
                            { head: "A"
                            , tail:
                                RecListStr $ V.inj (Proxy :: _ "cons")
                                  { head: "B"
                                  , tail: RecListStr $ V.inj (Proxy :: _ "nil") {}
                                  }
                            }
                        )
                    ]
                ]
                <#> printTsProgram
            )
            ( Right $ Map.fromFoldable
                [ textFile "Foo.Bar/index.d.ts"
                    [ "export const someList : import('../Data.RecListStr').RecListStr"
                    ]
                , textFile "Data.RecListStr/index.d.ts"
                    [ fold
                        [ "export type RecListStr = "
                        , "({ readonly 'type': 'cons'; readonly 'value': { readonly 'head': string; readonly 'tail': import('../Data.RecListStr').RecListStr; }; })"
                        , " | "
                        , "({ readonly 'type': 'nil'; readonly 'value': Record<string, never>; })"
                        ]
                    ]
                ]
            )

      describe "TsRecord" do
        it "should work with an empty TsRecord" do
          shouldEqual
            ( TSB.tsProgram
                [ TSB.tsModuleFile "Foo.Bar"
                    [ TSB.tsTypeAlias TSB.Tok "SomeRecord"
                        (Proxy :: _ (TsRecord ()))
                    ]
                ]
                <#> printTsProgram
            )
            ( Right $ Map.fromFoldable
                [ textFile "Foo.Bar/index.d.ts"
                    [ "export type SomeRecord = Record<string, never>"
                    ]
                ]
            )
        it "should work with no modifiers" do
          shouldEqual
            ( TSB.tsProgram
                [ TSB.tsModuleFile "Foo.Bar"
                    [ TSB.tsTypeAlias TSB.Tok "SomeRecord"
                        (Proxy :: _ (TsRecord (field1 :: Mod () Number)))
                    ]
                ]
                <#> printTsProgram
            )
            ( Right $ Map.fromFoldable
                [ textFile "Foo.Bar/index.d.ts"
                    [ "export type SomeRecord = { 'field1': number; }"
                    ]
                ]
            )
        it "should work with optional modifier" do
          shouldEqual
            ( TSB.tsProgram
                [ TSB.tsModuleFile "Foo.Bar"
                    [ TSB.tsTypeAlias TSB.Tok "SomeRecord"
                        ( Proxy
                            :: _
                                 ( TsRecord
                                     ( field1 :: Mod (optional :: True) Number
                                     , field2 :: Mod (optional :: False) Number
                                     )
                                 )
                        )
                    ]
                ]
                <#> printTsProgram
            )
            ( Right $ Map.fromFoldable
                [ textFile "Foo.Bar/index.d.ts"
                    [ "export type SomeRecord = { 'field1'?: number; 'field2': number; }"
                    ]
                ]
            )

        it "it should work with readonly modifier" do
          shouldEqual
            ( TSB.tsProgram
                [ TSB.tsModuleFile "Foo.Bar"
                    [ TSB.tsTypeAlias TSB.Tok "SomeRecord"
                        ( Proxy
                            :: _
                                 ( TsRecord
                                     ( field1 :: Mod (readonly :: True) Number
                                     , field2 :: Mod (readonly :: False) Number
                                     )
                                 )
                        )
                    ]
                ]
                <#> printTsProgram
            )
            ( Right $ Map.fromFoldable
                [ textFile "Foo.Bar/index.d.ts"
                    [ "export type SomeRecord = { readonly 'field1': number; 'field2': number; }"
                    ]
                ]
            )

        it "should work with readonly and optional" do
          shouldEqual
            ( TSB.tsProgram
                [ TSB.tsModuleFile "Foo.Bar"
                    [ TSB.tsTypeAlias TSB.Tok "SomeRecord"
                        ( Proxy
                            :: _
                                 ( TsRecord
                                     ( field1 :: Mod (readonly :: True, optional :: True) Number
                                     )
                                 )
                        )
                    ]
                ]
                <#> printTsProgram
            )
            ( Right $ Map.fromFoldable
                [ textFile "Foo.Bar/index.d.ts"
                    [ "export type SomeRecord = { readonly 'field1'?: number; }"
                    ]
                ]
            )

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

testTypePrint' :: TsBridgeM DTS.TsType -> String -> Spec Unit
testTypePrint' x s =
  it "prints the correct type" do
    U.shouldEqual
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