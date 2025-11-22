module TsBridge.DefaultImpls
  ( class NTupleList
  , class TsBridgeRecord
  , class TsBridgeRecordRL
  , class TsBridgeVariant
  , class TsBridgeVariantEncodedFlat
  , class TsBridgeVariantEncodedFlatRL
  , class TsBridgeVariantEncodedNested
  , class TsBridgeVariantEncodedNestedRL
  , class TsBridgeVariantRL
  , tsBridgeArray
  , tsBridgeBoolean
  , tsBridgeBooleanLit'
  , tsBridgeBooleanLitFalse
  , tsBridgeBooleanLitTrue
  , tsBridgeChar
  , tsBridgeEffect
  , tsBridgeEffectFn1
  , tsBridgeEffectFn2
  , tsBridgeEffectFn3
  , tsBridgeEffectFn4
  , tsBridgeEither
  , tsBridgeFn2
  , tsBridgeFn3
  , tsBridgeFn4
  , tsBridgeFunction
  , tsBridgeInt
  , tsBridgeIntLit'
  , tsBridgeMaybe
  , tsBridgeNTuple
  , tsBridgeNTupleList
  , tsBridgeNewtype0
  , tsBridgeNewtype1
  , tsBridgeNewtype2
  , tsBridgeNewtype3
  , tsBridgeNewtype4
  , tsBridgeNull
  , tsBridgeNullable
  , tsBridgeNumber
  , tsBridgeObject
  , tsBridgeOneOf
  , tsBridgeOpaqueType
  , tsBridgePromise
  , tsBridgeQualNamed
  , tsBridgeRecord
  , tsBridgeRecordRL
  , tsBridgeString
  , tsBridgeStringLit
  , tsBridgeStringLit'
  , tsBridgeTuple
  , tsBridgeTypeVar
  , tsBridgeUndefined
  , tsBridgeUnit
  , tsBridgeVariant
  , tsBridgeVariantEncodedFlat
  , tsBridgeVariantEncodedFlatRL
  , tsBridgeVariantEncodedNested
  , tsBridgeVariantEncodedNestedRL
  , tsBridgeVariantRL
  , tsBridgeNamed
  , module Exp
  ) where

import Prelude

import Control.Monad.Writer (censor, listen, tell)
import Control.Promise (Promise)
import DTS (OSet(..))
import DTS as DTS
import Data.Array (mapWithIndex, (:))
import Data.Array as A
import Data.Either (Either)
import Data.Function.Uncurried (Fn2, Fn3, Fn4)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, over)
import Data.Nullable (Nullable)
import Data.Reflectable (class Reflectable, reflectType)
import Data.Set as Set
import Data.Set.Ordered as OSet
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Data.Variant.Encodings.Flat (VariantEncodedFlat)
import Data.Variant.Encodings.Nested (VariantEncodedNested)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4)
import Foreign.Object (Object)
import Literals (StringLit, BooleanLit)
import Literals.Null (Null)
import Literals.Undefined as Lit
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Record as R
import Safe.Coerce (coerce)
import TsBridge.Core (class TsBridgeBy, tsBridgeBy)
import TsBridge.Monad (Scope(..), TsBridgeAccum(..), TsBridgeM, getAccum)
import TsBridge.Types (AppError(..), mapErr, mkName, toTsName)
import TsBridge.Types.Lit (Lit)
import TsBridge.Types.NTuple (NTuple)
import TsBridge.Types.Named (class GetName, Named, QualNamed, getName)
import TsBridge.Types.TypeVar (TypeVar)
import TsBridge.Types.TypeVar (TypeVar) as Exp
import Type.Data.List (type (:>), List', Nil')
import Type.Proxy (Proxy(..))
import Untagged.Union (OneOf)

-------------------------------------------------------------------------------
-- tsBridge methods
-------------------------------------------------------------------------------

tsBridgeNamed :: forall tok sym a. TsBridgeBy tok a => tok -> Proxy (Named sym a) -> TsBridgeM DTS.TsType
tsBridgeNamed tok _ = tsBridgeBy tok (Proxy :: _ a)

tsBridgeQualNamed
  :: forall tok moduleName typeName a
   . IsSymbol moduleName
  => IsSymbol typeName
  => TsBridgeBy tok a
  => tok
  -> Proxy (QualNamed moduleName typeName a)
  -> TsBridgeM DTS.TsType
tsBridgeQualNamed tok = tsBridgeNewtype0 tok
  { moduleName: reflectSymbol (Proxy :: _ moduleName)
  , typeName: reflectSymbol (Proxy :: _ typeName)
  }

-- | `tsBridge` type class method implementation for type variables.
-- | This is needed because polymorphic values cannot be exported directly.
-- | They have to be monomorphized: E.g. something of type `Maybe a` needs to be
-- | typed `Maybe (Var "A")` to be exported.
tsBridgeTypeVar :: forall s. IsSymbol s => Proxy (TypeVar s) -> TsBridgeM DTS.TsType
tsBridgeTypeVar _ = do

  name <- mkName $ reflectSymbol (Proxy :: _ s)

  let
    scope = Scope
      { floating: coerce $ OSet.singleton $ toTsName name
      , fixed: OSet.empty
      }

  tell
    $ over TsBridgeAccum _ { scope = scope } mempty
  pure $ DTS.TsTypeVar $ toTsName name

-- | `tsBridge` type class method implementation for the `Number` type.
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#number)
-- | for details.
tsBridgeNumber :: Proxy Number -> TsBridgeM DTS.TsType
tsBridgeNumber _ = pure DTS.TsTypeNumber

-- | `tsBridge` type class method implementation for the `String` type
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#string)
-- | for details.
tsBridgeString :: Proxy String -> TsBridgeM DTS.TsType
tsBridgeString _ = pure DTS.TsTypeString

-- | `tsBridge` type class method implementation for the `Boolean` type
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#boolean)
-- | for details.
tsBridgeBoolean :: Proxy Boolean -> TsBridgeM DTS.TsType
tsBridgeBoolean _ = pure DTS.TsTypeBoolean

-- | `tsBridge` type class method implementation for the `Int` type
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#int)
-- | for details.
tsBridgeInt :: Proxy Int -> TsBridgeM DTS.TsType
tsBridgeInt = tsBridgeOpaqueType
  { moduleName: "Prim"
  , typeName: "Int"
  , typeArgs: []
  }

-- | `tsBridge` type class method implementation for the `Char` type
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#char)
-- | for details.
tsBridgeChar :: Proxy Char -> TsBridgeM DTS.TsType
tsBridgeChar = tsBridgeOpaqueType
  { moduleName: "Prim"
  , typeName: "Char"
  , typeArgs: []
  }

-- | `tsBridge` type class method implementation for the `Unit` type
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#unit)
-- | for details.
tsBridgeUnit :: Proxy Unit -> TsBridgeM DTS.TsType
tsBridgeUnit _ = pure DTS.TsTypeVoid

-- | `tsBridge` type class method implementation for the `Effect` type
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#number)
-- | for details.
tsBridgeEffect :: forall tok a. TsBridgeBy tok a => tok -> Proxy (Effect a) -> TsBridgeM DTS.TsType
tsBridgeEffect tok _ = censor mapAccum ado
  ret /\ TsBridgeAccum { scope: Scope scopeRet } <- listen $ tsBridgeBy tok (Proxy :: _ a)
  let
    newFixed = (scopeRet.fixed)
      <> scopeRet.floating

    removeQuant =
      DTS.mapQuantifier $ coerce $ OSet.filter (_ `OSet.notElem` newFixed)

  in
    DTS.TsTypeFunction (DTS.TsTypeArgsQuant $ coerce newFixed) []
      (removeQuant ret)
  where
  mapAccum = over TsBridgeAccum (\x -> x { scope = fixScope x.scope })

-- | `tsBridge` type class method implementation for the `Array` type
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#array)
-- | for details.
tsBridgeArray :: forall a tok. TsBridgeBy tok a => tok -> Proxy (Array a) -> TsBridgeM DTS.TsType
tsBridgeArray tok _ = DTS.TsTypeArray <$> tsBridgeBy tok (Proxy :: _ a)

tsBridgeObject :: forall tok a. TsBridgeBy tok a => tok -> Proxy (Object a) -> TsBridgeM DTS.TsType
tsBridgeObject tok _ = do
  x <- tsBridgeBy tok (Proxy :: _ a)
  pure $ DTS.TsTypeConstructor
    (DTS.TsQualName Nothing $ DTS.TsName "Record")
    (DTS.TsTypeArgs [ DTS.TsTypeString, x ])

-- | tsBridge type class method implementation for the `Tuple` type
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#tuple)
-- | for details.
tsBridgeTuple
  :: forall tok a b
   . TsBridgeBy tok a
  => TsBridgeBy tok b
  => tok
  -> Proxy (Tuple a b)
  -> TsBridgeM DTS.TsType
tsBridgeTuple tok =
  tsBridgeOpaqueType
    { moduleName: "Data.Tuple"
    , typeName: "Tuple"
    , typeArgs:
        [ "A" /\ tsBridgeBy tok (Proxy :: _ a)
        , "B" /\ tsBridgeBy tok (Proxy :: _ b)
        ]
    }

-- | `tsBridge` type class method implementation for the `Either` type
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#either)
-- | for details.
-- | 
tsBridgeEither
  :: forall tok a b
   . TsBridgeBy tok a
  => TsBridgeBy tok b
  => tok
  -> Proxy (Either a b)
  -> TsBridgeM DTS.TsType
tsBridgeEither tok =
  tsBridgeOpaqueType
    { moduleName: "Data.Either"
    , typeName: "Either"
    , typeArgs:
        [ "A" /\ tsBridgeBy tok (Proxy :: _ a)
        , "B" /\ tsBridgeBy tok (Proxy :: _ b)
        ]
    }

-- | `tsBridge` type class method implementation for the `Maybe` type
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#maybe)
-- | for details.
tsBridgeMaybe :: forall tok a. TsBridgeBy tok a => tok -> Proxy (Maybe a) -> TsBridgeM DTS.TsType
tsBridgeMaybe tok =
  tsBridgeOpaqueType
    { moduleName: "Data.Maybe"
    , typeName: "Maybe"
    , typeArgs:
        [ "A" /\ tsBridgeBy tok (Proxy :: _ a)
        ]
    }

-- | `tsBridge` type class method implementation for the `Promise` type.
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#promise)
-- | for details.
tsBridgePromise :: forall tok a. TsBridgeBy tok a => tok -> Proxy (Promise a) -> TsBridgeM DTS.TsType
tsBridgePromise tok _ = do
  x <- tsBridgeBy tok (Proxy :: _ a)
  pure $ DTS.TsTypeConstructor
    (DTS.TsQualName Nothing (DTS.TsName "Promise"))
    (DTS.TsTypeArgs [ x ])

-- | `tsBridge` type class method implementation for the `Nullable` type. 
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#number)
-- | for details.
tsBridgeNullable :: forall a tok. TsBridgeBy tok a => tok -> Proxy (Nullable a) -> TsBridgeM DTS.TsType
tsBridgeNullable tok _ = do
  x <- tsBridgeBy tok (Proxy :: _ a)
  pure $ DTS.TsTypeUnion
    [ DTS.TsTypeNull, x ]

-- | `tsBridge` type class method implementation for the `OneOf` type. 
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#oneof)
-- | for details.
tsBridgeOneOf
  :: forall a b tok
   . TsBridgeBy tok a
  => TsBridgeBy tok b
  => tok
  -> Proxy (OneOf a b)
  -> TsBridgeM DTS.TsType
tsBridgeOneOf tok _ = do
  x <- tsBridgeBy tok (Proxy :: _ a)
  y <- tsBridgeBy tok (Proxy :: _ b)

  pure $ DTS.TsTypeUnion [ x, y ]

-- | `tsBridge` type class method implementation for the `Undefined` type.
tsBridgeUndefined :: Proxy Lit.Undefined -> TsBridgeM DTS.TsType
tsBridgeUndefined _ = pure $ DTS.TsTypeVar (DTS.TsName "undefined")

tsBridgeNull :: Proxy Null -> TsBridgeM DTS.TsType
tsBridgeNull _ = pure DTS.TsTypeNull

-- | `tsBridge` type class method implementation for string literal types.
tsBridgeStringLit :: forall sym. IsSymbol sym => Proxy (StringLit sym) -> TsBridgeM DTS.TsType
tsBridgeStringLit _ = pure $ DTS.TsTypeTypelevelString $ reflectSymbol (Proxy :: _ sym)

tsBridgeBooleanLitTrue :: Proxy (BooleanLit "true") -> TsBridgeM DTS.TsType
tsBridgeBooleanLitTrue _ = pure $ DTS.TsTypeVar (DTS.TsName "true")

tsBridgeBooleanLitFalse :: Proxy (BooleanLit "false") -> TsBridgeM DTS.TsType
tsBridgeBooleanLitFalse _ = pure $ DTS.TsTypeVar (DTS.TsName "false")

tsBridgeStringLit' :: forall h. Reflectable h String => Proxy (Lit h String) -> TsBridgeM DTS.TsType
tsBridgeStringLit' _ = pure $ DTS.TsTypeTypelevelString $ (reflectType @h Proxy)

tsBridgeBooleanLit' :: forall h. Reflectable h Boolean => Proxy (Lit h Boolean) -> TsBridgeM DTS.TsType
tsBridgeBooleanLit' _ = pure $ DTS.TsTypeVar $ DTS.TsName
  (if reflectType @h Proxy then "true" else "false")

tsBridgeIntLit' :: forall h. Reflectable h Int => Proxy (Lit h Int) -> TsBridgeM DTS.TsType
tsBridgeIntLit' _ = pure $ DTS.TsTypeTypelevelNumber $ (Int.toNumber $ reflectType @h Proxy)

-------------------------------------------------------------------------------
-- tsBridge methods / class VariantEncodedFlat 
-------------------------------------------------------------------------------

class TsBridgeVariantEncodedFlat :: Type -> Symbol -> Row Type -> Constraint
class TsBridgeVariantEncodedFlat tok symTag r where
  -- | `tsBridge` type class method implementation for the `TsBridgeVariantEncodedFlat` type
  -- |
  -- | See [this
  -- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#VariantEncodedFlat)
  -- | for details.
  tsBridgeVariantEncodedFlat :: tok -> Proxy (VariantEncodedFlat symTag r) -> TsBridgeM DTS.TsType

instance (RowToList r rl, TsBridgeVariantEncodedFlatRL tok symTag rl) => TsBridgeVariantEncodedFlat tok symTag r where
  tsBridgeVariantEncodedFlat tok _ = DTS.TsTypeUnion <$> tsBridgeVariantEncodedFlatRL tok (Proxy :: _ symTag) (Proxy :: _ rl)

-------------------------------------------------------------------------------
-- tsBridge methods / class TsBridgeVariantRL
-------------------------------------------------------------------------------

class TsBridgeVariantEncodedFlatRL :: Type -> Symbol -> RowList Type -> Constraint
class TsBridgeVariantEncodedFlatRL tok symTag rl where
  -- | `tsBridge` type class method implementation for the `TsBridgeVariantEncodedNested` type
  -- |
  -- | See [this
  -- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#VariantEncodedNested)
  -- | for details.
  tsBridgeVariantEncodedFlatRL :: tok -> Proxy symTag -> Proxy rl -> TsBridgeM (Array DTS.TsType)

instance TsBridgeVariantEncodedFlatRL tok symTag Nil where
  tsBridgeVariantEncodedFlatRL _ _ _ = pure []

instance
  ( TsBridgeBy tok a
  , TsBridgeVariantEncodedFlatRL tok symTag rl
  , IsSymbol s
  , IsSymbol symTag
  ) =>
  TsBridgeVariantEncodedFlatRL tok symTag (Cons s a rl) where
  tsBridgeVariantEncodedFlatRL tok prxSymTag _ =
    do
      x <- tsBridgeBy tok (Proxy :: _ a)
      xs <- tsBridgeVariantEncodedFlatRL tok prxSymTag (Proxy :: _ rl)
      pure $
        A.cons
          ( DTS.TsTypeIntersection
              [ DTS.TsTypeRecord
                  [ DTS.TsRecordField (reflectSymbol prxSymTag)
                      { readonly: true, optional: false }
                      (DTS.TsTypeTypelevelString $ reflectSymbol (Proxy :: _ s))
                  ]
              , x
              ]
          )
          xs

--------------------------------------------------------------------------------

-- | `tsBridge` type class method implementation for the `a -> b` (Function) type
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#function)
-- | for details.
tsBridgeFunction
  :: forall tok a b
   . TsBridgeBy tok a
  => TsBridgeBy tok b
  => GetName a
  => tok
  -> Proxy (a -> b)
  -> TsBridgeM DTS.TsType
tsBridgeFunction tok _ = censor mapAccum ado
  arg /\ TsBridgeAccum { scope: Scope scopeArg } <- listen $ tsBridgeBy tok (Proxy :: _ a)
  ret /\ TsBridgeAccum { scope: Scope scopeRet } <- listen $ tsBridgeBy tok (Proxy :: _ b)
  let
    newFixed = (OSet.intersect scopeArg.fixed scopeRet.fixed)
      <> scopeArg.floating
      <> scopeRet.floating

    removeQuant =
      DTS.mapQuantifier $ coerce $ OSet.filter (_ `OSet.notElem` newFixed)

    argName = fromMaybe "_" $ getName (Proxy :: _ a)

  in
    DTS.TsTypeFunction (DTS.TsTypeArgsQuant $ coerce newFixed)
      [ DTS.TsFnArg (DTS.TsName argName) (removeQuant arg)
      ]
      (removeQuant ret)
  where
  mapAccum = over TsBridgeAccum (\x -> x { scope = fixScope x.scope })

tsBridgeFn2
  :: forall tok a1 a2 b
   . TsBridgeBy tok a1
  => TsBridgeBy tok a2
  => TsBridgeBy tok b
  => GetName a1
  => GetName a2
  => tok
  -> Proxy (Fn2 a1 a2 b)
  -> TsBridgeM DTS.TsType
tsBridgeFn2 tok _ = censor mapAccum ado
  arg1 /\ TsBridgeAccum { scope: Scope scopeArg1 } <- listen $ tsBridgeBy tok (Proxy :: _ a1)
  arg2 /\ TsBridgeAccum { scope: Scope scopeArg2 } <- listen $ tsBridgeBy tok (Proxy :: _ a2)
  ret /\ TsBridgeAccum { scope: Scope scopeRet } <- listen $ tsBridgeBy tok (Proxy :: _ b)
  let
    newFixed =
      ( scopeRet.fixed
          # OSet.intersect scopeArg1.fixed
          # OSet.intersect scopeArg2.fixed
      )
        <> scopeArg1.floating
        <> scopeArg2.floating
        <> scopeRet.floating

    removeQuant =
      DTS.mapQuantifier $ coerce $ OSet.filter (_ `OSet.notElem` newFixed)

    arg1Name = fromMaybe "arg1" $ getName (Proxy :: _ a1)
    arg2Name = fromMaybe "arg2" $ getName (Proxy :: _ a2)

  in
    DTS.TsTypeFunction (DTS.TsTypeArgsQuant $ coerce newFixed)
      [ DTS.TsFnArg (DTS.TsName arg1Name) (removeQuant arg1)
      , DTS.TsFnArg (DTS.TsName arg2Name) (removeQuant arg2)
      ]
      (removeQuant ret)
  where
  mapAccum = over TsBridgeAccum (\x -> x { scope = fixScope x.scope })

tsBridgeFn3
  :: forall tok a1 a2 a3 b
   . TsBridgeBy tok a1
  => TsBridgeBy tok a2
  => TsBridgeBy tok a3
  => TsBridgeBy tok b
  => GetName a1
  => GetName a2
  => GetName a3
  => tok
  -> Proxy (Fn3 a1 a2 a3 b)
  -> TsBridgeM DTS.TsType
tsBridgeFn3 tok _ = censor mapAccum ado
  arg1 /\ TsBridgeAccum { scope: Scope scopeArg1 } <- listen $ tsBridgeBy tok (Proxy :: _ a1)
  arg2 /\ TsBridgeAccum { scope: Scope scopeArg2 } <- listen $ tsBridgeBy tok (Proxy :: _ a2)
  arg3 /\ TsBridgeAccum { scope: Scope scopeArg3 } <- listen $ tsBridgeBy tok (Proxy :: _ a3)
  ret /\ TsBridgeAccum { scope: Scope scopeRet } <- listen $ tsBridgeBy tok (Proxy :: _ b)
  let
    newFixed =
      ( scopeRet.fixed
          # OSet.intersect scopeArg1.fixed
          # OSet.intersect scopeArg2.fixed
          # OSet.intersect scopeArg3.fixed
      )
        <> scopeArg1.floating
        <> scopeArg2.floating
        <> scopeArg3.floating
        <> scopeRet.floating

    removeQuant =
      DTS.mapQuantifier $ coerce $ OSet.filter (_ `OSet.notElem` newFixed)

    arg1Name = fromMaybe "arg1" $ getName (Proxy :: _ a1)
    arg2Name = fromMaybe "arg2" $ getName (Proxy :: _ a2)
    arg3Name = fromMaybe "arg3" $ getName (Proxy :: _ a3)

  in
    DTS.TsTypeFunction (DTS.TsTypeArgsQuant $ coerce newFixed)
      [ DTS.TsFnArg (DTS.TsName arg1Name) (removeQuant arg1)
      , DTS.TsFnArg (DTS.TsName arg2Name) (removeQuant arg2)
      , DTS.TsFnArg (DTS.TsName arg3Name) (removeQuant arg3)
      ]
      (removeQuant ret)
  where
  mapAccum = over TsBridgeAccum (\x -> x { scope = fixScope x.scope })

tsBridgeFn4
  :: forall tok a1 a2 a3 a4 b
   . TsBridgeBy tok a1
  => TsBridgeBy tok a2
  => TsBridgeBy tok a3
  => TsBridgeBy tok a4
  => TsBridgeBy tok b
  => GetName a1
  => GetName a2
  => GetName a3
  => GetName a4
  => tok
  -> Proxy (Fn4 a1 a2 a3 a4 b)
  -> TsBridgeM DTS.TsType
tsBridgeFn4 tok _ = censor mapAccum ado
  arg1 /\ TsBridgeAccum { scope: Scope scopeArg1 } <- listen $ tsBridgeBy tok (Proxy :: _ a1)
  arg2 /\ TsBridgeAccum { scope: Scope scopeArg2 } <- listen $ tsBridgeBy tok (Proxy :: _ a2)
  arg3 /\ TsBridgeAccum { scope: Scope scopeArg3 } <- listen $ tsBridgeBy tok (Proxy :: _ a3)
  arg4 /\ TsBridgeAccum { scope: Scope scopeArg4 } <- listen $ tsBridgeBy tok (Proxy :: _ a4)
  ret /\ TsBridgeAccum { scope: Scope scopeRet } <- listen $ tsBridgeBy tok (Proxy :: _ b)
  let
    newFixed =
      ( scopeRet.fixed
          # OSet.intersect scopeArg1.fixed
          # OSet.intersect scopeArg2.fixed
          # OSet.intersect scopeArg3.fixed
          # OSet.intersect scopeArg4.fixed
      )
        <> scopeArg1.floating
        <> scopeArg2.floating
        <> scopeArg3.floating
        <> scopeArg4.floating
        <> scopeRet.floating

    removeQuant =
      DTS.mapQuantifier $ coerce $ OSet.filter (_ `OSet.notElem` newFixed)

    arg1Name = fromMaybe "arg1" $ getName (Proxy :: _ a1)
    arg2Name = fromMaybe "arg2" $ getName (Proxy :: _ a2)
    arg3Name = fromMaybe "arg3" $ getName (Proxy :: _ a3)
    arg4Name = fromMaybe "arg4" $ getName (Proxy :: _ a4)

  in
    DTS.TsTypeFunction (DTS.TsTypeArgsQuant $ coerce newFixed)
      [ DTS.TsFnArg (DTS.TsName arg1Name) (removeQuant arg1)
      , DTS.TsFnArg (DTS.TsName arg2Name) (removeQuant arg2)
      , DTS.TsFnArg (DTS.TsName arg3Name) (removeQuant arg3)
      , DTS.TsFnArg (DTS.TsName arg4Name) (removeQuant arg4)
      ]
      (removeQuant ret)
  where
  mapAccum = over TsBridgeAccum (\x -> x { scope = fixScope x.scope })

tsBridgeEffectFn1
  :: forall tok a1 b
   . TsBridgeBy tok a1
  => TsBridgeBy tok b
  => GetName a1
  => tok
  -> Proxy (EffectFn1 a1 b)
  -> TsBridgeM DTS.TsType
tsBridgeEffectFn1 tok _ = tsBridgeFunction tok (Proxy :: _ (a1 -> b))

tsBridgeEffectFn2
  :: forall tok a1 a2 b
   . TsBridgeBy tok a1
  => TsBridgeBy tok a2
  => TsBridgeBy tok b
  => GetName a1
  => GetName a2
  => tok
  -> Proxy (EffectFn2 a1 a2 b)
  -> TsBridgeM DTS.TsType
tsBridgeEffectFn2 tok _ = tsBridgeFn2 tok (Proxy :: _ (Fn2 a1 a2 b))

tsBridgeEffectFn3
  :: forall tok a1 a2 a3 b
   . TsBridgeBy tok a1
  => TsBridgeBy tok a2
  => TsBridgeBy tok a3
  => TsBridgeBy tok b
  => GetName a1
  => GetName a2
  => GetName a3
  => tok
  -> Proxy (EffectFn3 a1 a2 a3 b)
  -> TsBridgeM DTS.TsType
tsBridgeEffectFn3 tok _ = tsBridgeFn3 tok (Proxy :: _ (Fn3 a1 a2 a3 b))

tsBridgeEffectFn4
  :: forall tok a1 a2 a3 a4 b
   . TsBridgeBy tok a1
  => TsBridgeBy tok a2
  => TsBridgeBy tok a3
  => TsBridgeBy tok a4
  => TsBridgeBy tok b
  => GetName a1
  => GetName a2
  => GetName a3
  => GetName a4
  => tok
  -> Proxy (EffectFn4 a1 a2 a3 a4 b)
  -> TsBridgeM DTS.TsType
tsBridgeEffectFn4 tok _ = tsBridgeFn4 tok (Proxy :: _ (Fn4 a1 a2 a3 a4 b))

-- | `tsBridge` type class method implementation for opaque types
tsBridgeOpaqueType :: forall a. { moduleName :: String, typeName :: String, typeArgs :: Array (String /\ TsBridgeM DTS.TsType) } -> Proxy a -> TsBridgeM DTS.TsType
tsBridgeOpaqueType { moduleName, typeName, typeArgs } _ =
  mapErr (AtType typeName)
    do
      argNames <- typeArgs <#> fst # traverse mkName
      targs <- typeArgs <#> snd # sequence
      name <- mkName typeName

      let
        filePath = DTS.TsFilePath (moduleName <> "/index.d.ts")
        importPath = DTS.TsImportPath ("../" <> moduleName)

        typeDefs =
          [ DTS.TsModuleFile
              filePath
              ( DTS.TsModule
                  [ mkBrandedTypeDecl (toTsName name) (coerce $ OSet.fromFoldable $ toTsName <$> argNames) Nothing
                  ]
              )
          ]

      tell
        $ TsBridgeAccum
        $ R.union mempty { typeDefs }

      pure
        $ DTS.TsTypeConstructor (DTS.TsQualName (Just importPath) (toTsName name)) (DTS.TsTypeArgs targs)

tsBridgeNewtype0
  :: forall tok a i i2
   . Newtype a i
  => Newtype (a) i2
  => TsBridgeBy tok i2
  => tok
  -> { moduleName :: String
     , typeName :: String
     }
  -> Proxy a
  -> TsBridgeM DTS.TsType
tsBridgeNewtype0 tok { moduleName, typeName } _ =
  mapErr (AtType typeName)
    do
      let
        targNames = []
        targs = []
      newtypeImpl tok moduleName typeName targs targNames (Proxy :: _ i2)

tsBridgeNewtype1
  :: forall @sym1 tok a t1 i i2
   . Newtype (a t1) i
  => Newtype (a (TypeVar sym1)) i2
  => TsBridgeBy tok i2
  => TsBridgeBy tok (TypeVar sym1)
  => IsSymbol sym1
  => tok
  -> { moduleName :: String
     , typeName :: String
     }
  -> Proxy (a t1)
  -> TsBridgeM DTS.TsType
tsBridgeNewtype1 tok { moduleName, typeName } _ =
  mapErr (AtType typeName)
    do
      let
        targNames = [ reflectSymbol (Proxy :: _ sym1) ]
        targs = [ tsBridgeBy tok (Proxy :: _ (TypeVar sym1)) ]

      newtypeImpl tok moduleName typeName targs targNames (Proxy :: _ i2)

tsBridgeNewtype2
  :: forall @sym1 @sym2 tok a t1 t2 i i2
   . Newtype (a t1 t2) i
  => Newtype (a (TypeVar sym1) (TypeVar sym2)) i2
  => TsBridgeBy tok i2
  => TsBridgeBy tok (TypeVar sym1)
  => TsBridgeBy tok (TypeVar sym2)
  => IsSymbol sym1
  => IsSymbol sym2
  => tok
  -> { moduleName :: String
     , typeName :: String
     }
  -> Proxy (a t1 t2)
  -> TsBridgeM DTS.TsType
tsBridgeNewtype2 tok { moduleName, typeName } _ =
  mapErr (AtType typeName)
    do
      let
        targNames = [ reflectSymbol (Proxy :: _ sym1), reflectSymbol (Proxy :: _ sym2) ]
        targs = [ tsBridgeBy tok (Proxy :: _ (TypeVar sym1)), tsBridgeBy tok (Proxy :: _ (TypeVar sym2)) ]

      newtypeImpl tok moduleName typeName targs targNames (Proxy :: _ i2)

tsBridgeNewtype3
  :: forall @sym1 @sym2 @sym3 tok a t1 t2 t3 i i2
   . Newtype (a t1 t2 t3) i
  => Newtype (a (TypeVar sym1) (TypeVar sym2) (TypeVar sym3)) i2
  => TsBridgeBy tok i2
  => TsBridgeBy tok (TypeVar sym1)
  => TsBridgeBy tok (TypeVar sym2)
  => TsBridgeBy tok (TypeVar sym3)
  => IsSymbol sym1
  => IsSymbol sym2
  => IsSymbol sym3
  => tok
  -> { moduleName :: String
     , typeName :: String
     }
  -> Proxy (a t1 t2 t3)
  -> TsBridgeM DTS.TsType
tsBridgeNewtype3 tok { moduleName, typeName } _ =
  mapErr (AtType typeName)
    do
      let
        targNames = [ reflectSymbol (Proxy :: _ sym1), reflectSymbol (Proxy :: _ sym2), reflectSymbol (Proxy :: _ sym3) ]
        targs = [ tsBridgeBy tok (Proxy :: _ (TypeVar sym1)), tsBridgeBy tok (Proxy :: _ (TypeVar sym2)), tsBridgeBy tok (Proxy :: _ (TypeVar sym3)) ]

      newtypeImpl tok moduleName typeName targs targNames (Proxy :: _ i2)

tsBridgeNewtype4
  :: forall @sym1 @sym2 @sym3 @sym4 tok a t1 t2 t3 t4 i i2
   . Newtype (a t1 t2 t3 t4) i
  => Newtype (a (TypeVar sym1) (TypeVar sym2) (TypeVar sym3) (TypeVar sym4)) i2
  => TsBridgeBy tok i2
  => TsBridgeBy tok (TypeVar sym1)
  => TsBridgeBy tok (TypeVar sym2)
  => TsBridgeBy tok (TypeVar sym3)
  => TsBridgeBy tok (TypeVar sym4)
  => IsSymbol sym1
  => IsSymbol sym2
  => IsSymbol sym3
  => IsSymbol sym4
  => tok
  -> { moduleName :: String
     , typeName :: String
     }
  -> Proxy (a t1 t2 t3 t4)
  -> TsBridgeM DTS.TsType
tsBridgeNewtype4 tok { moduleName, typeName } _ =
  mapErr (AtType typeName)
    do
      let
        targNames =
          [ reflectSymbol (Proxy :: _ sym1)
          , reflectSymbol (Proxy :: _ sym2)
          , reflectSymbol (Proxy :: _ sym3)
          , reflectSymbol (Proxy :: _ sym4)
          ]
        targs =
          [ tsBridgeBy tok (Proxy :: _ (TypeVar sym1))
          , tsBridgeBy tok (Proxy :: _ (TypeVar sym2))
          , tsBridgeBy tok (Proxy :: _ (TypeVar sym3))
          , tsBridgeBy tok (Proxy :: _ (TypeVar sym4))
          ]

      newtypeImpl tok moduleName typeName targs targNames (Proxy :: _ i2)

newtypeImpl
  :: forall tok i2
   . TsBridgeBy tok i2
  => tok
  -> String
  -> String
  -> Array (TsBridgeM DTS.TsType)
  -> Array String
  -> Proxy i2
  -> TsBridgeM DTS.TsType
newtypeImpl tok moduleName typeName targs targNames _ = do
  let
    filePath = DTS.TsFilePath (moduleName <> "/index.d.ts")
    filePathRef = DTS.TsImportPath ("../" <> moduleName)

  name <- mkName typeName
  args <- sequence targs

  TsBridgeAccum accum <- getAccum

  unless (Set.member { moduleName, typeName } accum.registeredTypes) do

    tell
      $ TsBridgeAccum
      $ R.union mempty { registeredTypes: Set.singleton { moduleName, typeName } }

    x <- tsBridgeBy tok (Proxy :: _ i2)

    args' <- OSet.fromFoldable <$> map toTsName <$> traverse mkName targNames

    let
      removeQuant =
        DTS.mapQuantifier $ coerce $ OSet.filter (_ `OSet.notElem` args')

    let
      typeDefs =
        [ DTS.TsModuleFile
            filePath
            ( DTS.TsModule
                [ DTS.TsDeclTypeDef (toTsName name) DTS.Public (coerce args') (removeQuant x)
                ]
            )
        ]

    tell
      $ TsBridgeAccum
      $ R.union mempty { typeDefs, registeredTypes: Set.singleton { moduleName, typeName } }

  pure
    $ DTS.TsTypeConstructor (DTS.TsQualName (Just filePathRef) (toTsName name)) (DTS.TsTypeArgs args)

fixNewtype :: DTS.TsType -> DTS.TsType
fixNewtype = case _ of
  DTS.TsTypeNumber -> DTS.TsTypeNumber
  DTS.TsTypeString -> DTS.TsTypeString
  DTS.TsTypeBoolean -> DTS.TsTypeBoolean
  DTS.TsTypeNull -> DTS.TsTypeNull
  DTS.TsTypeArray x -> DTS.TsTypeArray $ fixNewtype x
  DTS.TsTypeReadonlyArray x -> DTS.TsTypeReadonlyArray $ fixNewtype x
  DTS.TsTypeIntersection xs -> DTS.TsTypeIntersection
    (fixNewtype <$> xs)
  DTS.TsTypeUnion xs -> DTS.TsTypeUnion
    (fixNewtype <$> xs)
  DTS.TsTypeReadonlyTuple xs -> DTS.TsTypeReadonlyTuple
    (fixNewtype <$> xs)
  DTS.TsTypeTuple xs -> DTS.TsTypeTuple
    (fixNewtype <$> xs)
  DTS.TsTypeRecord x -> DTS.TsTypeRecord $ goTsRecordField <$> x
  DTS.TsTypeFunction x y z -> DTS.TsTypeFunction
    (goTsTypeArgsQuant x)
    (goTsFnArg <$> y)
    (fixNewtype z)
  DTS.TsTypeConstructor x y -> DTS.TsTypeConstructor
    --(goTsQualName x)
    (DTS.TsQualName Nothing (DTS.TsName "foo"))
    (goTsTypeArgs y)
  DTS.TsTypeUniqueSymbol -> DTS.TsTypeUniqueSymbol
  DTS.TsTypeVar x -> DTS.TsTypeVar $ goTsName x
  DTS.TsTypeVoid -> DTS.TsTypeVoid
  DTS.TsTypeTypelevelString x -> DTS.TsTypeTypelevelString x
  DTS.TsTypeTypelevelNumber x -> DTS.TsTypeTypelevelNumber x
  where
  goTsRecordField (DTS.TsRecordField x y z) = DTS.TsRecordField
    (goTsName x)
    (goPropModifiers y)
    (fixNewtype z)

  goTsTypeArgsQuant (DTS.TsTypeArgsQuant oset) = DTS.TsTypeArgsQuant oset

  goTsTypeArgs (DTS.TsTypeArgs x) = DTS.TsTypeArgs $ fixNewtype <$> x

  goTsFnArg (DTS.TsFnArg x y) = DTS.TsFnArg (goTsName x) (fixNewtype y)

  goTsQualName = identity

  goTsName = identity

  goPropModifiers = identity

-------------------------------------------------------------------------------

tsBridgeNTuple :: forall tok xs. NTupleList tok xs => tok -> Proxy (NTuple xs) -> TsBridgeM DTS.TsType
tsBridgeNTuple tok _ = DTS.TsTypeReadonlyTuple <$> tsBridgeNTupleList tok (Proxy :: _ xs)

class NTupleList tok (xs :: List' Type) where
  tsBridgeNTupleList :: tok -> Proxy xs -> TsBridgeM (Array DTS.TsType)

instance NTupleList tok Nil' where
  tsBridgeNTupleList _ _ = pure []

instance (TsBridgeBy tok a, NTupleList tok as) => NTupleList tok (a :> as) where
  tsBridgeNTupleList tok _ = do
    x <- tsBridgeBy tok (Proxy :: _ a)

    rest <- tsBridgeNTupleList tok (Proxy :: _ as)

    pure $ A.cons x rest

-------------------------------------------------------------------------------
-- tsBridge methods / class TsBridgeRecord
-------------------------------------------------------------------------------

class TsBridgeRecord :: Type -> Row Type -> Constraint
class TsBridgeRecord tok r where
  -- | `tsBridge` type class method implementation for the Record type
  -- |
  -- | See [this
  -- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#record)
  -- | for details.
  tsBridgeRecord :: tok -> Proxy (Record r) -> TsBridgeM DTS.TsType

instance (RowToList r rl, TsBridgeRecordRL tok rl) => TsBridgeRecord tok r where
  tsBridgeRecord tok _ = DTS.TsTypeRecord <$> tsBridgeRecordRL tok (Proxy :: _ rl)

-------------------------------------------------------------------------------
-- tsBridge methods / class TsBridgeRecordRL
-------------------------------------------------------------------------------

class TsBridgeRecordRL :: Type -> RowList Type -> Constraint
class TsBridgeRecordRL tok rl where
  tsBridgeRecordRL :: tok -> Proxy rl -> TsBridgeM (Array DTS.TsRecordField)

instance TsBridgeRecordRL tok Nil where
  tsBridgeRecordRL _ _ = pure []

instance
  ( TsBridgeBy tok t
  , TsBridgeRecordRL tok rl
  , IsSymbol s
  ) =>
  TsBridgeRecordRL tok (Cons s t rl) where
  tsBridgeRecordRL tok _ = do
    x <- tsBridgeBy tok (Proxy :: _ t)
    xs <- tsBridgeRecordRL tok (Proxy :: _ rl)
    let k = reflectSymbol (Proxy :: _ s)
    pure $
      A.cons (DTS.TsRecordField k { optional: false, readonly: true } x) xs

-------------------------------------------------------------------------------
-- tsBridge methods / class TsBridgeVariant
-------------------------------------------------------------------------------

class TsBridgeVariant :: Type -> Row Type -> Constraint
class TsBridgeVariant tok r where
  -- | `tsBridge` type class method implementation for the Variant type
  -- |
  -- | See [this
  -- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#variant)
  -- | for details.
  tsBridgeVariant :: tok -> Proxy (Variant r) -> TsBridgeM DTS.TsType

instance (RowToList r rl, TsBridgeVariantRL tok rl) => TsBridgeVariant tok r where
  tsBridgeVariant tok _ = DTS.TsTypeUnion <$> tsBridgeVariantRL tok (Proxy :: _ rl)

-------------------------------------------------------------------------------
-- tsBridge methods / class TsBridgeVariantRL
-------------------------------------------------------------------------------

class TsBridgeVariantRL :: Type -> RowList Type -> Constraint
class TsBridgeVariantRL tok rl where
  tsBridgeVariantRL :: tok -> Proxy rl -> TsBridgeM (Array DTS.TsType)

instance TsBridgeVariantRL tok Nil where
  tsBridgeVariantRL _ _ = pure []

instance
  ( TsBridgeBy tok t
  , TsBridgeVariantRL tok rl
  , IsSymbol s
  ) =>
  TsBridgeVariantRL tok (Cons s t rl) where
  tsBridgeVariantRL tok _ =
    do
      x <- tsBridgeBy tok (Proxy :: _ t)
      xs <- tsBridgeVariantRL tok (Proxy :: _ rl)
      pure $
        A.cons
          ( DTS.TsTypeRecord
              [ DTS.TsRecordField "type"
                  { readonly: true, optional: false }
                  (DTS.TsTypeTypelevelString $ reflectSymbol (Proxy :: _ s))
              , DTS.TsRecordField "value"
                  { readonly: true, optional: false }
                  x
              ]
          )
          xs

-------------------------------------------------------------------------------
-- tsBridge methods / class TsBridgeVariantEncodedNested
-------------------------------------------------------------------------------

class TsBridgeVariantEncodedNested :: Type -> Symbol -> Symbol -> Row Type -> Constraint
class TsBridgeVariantEncodedNested tok symTag symVal r where
  -- | `tsBridge` type class method implementation for the VariantEncodedNested type
  -- |
  -- | See [this
  -- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#VariantEncodedNested)
  -- | for details.
  tsBridgeVariantEncodedNested :: tok -> Proxy (VariantEncodedNested symTag symVal r) -> TsBridgeM DTS.TsType

instance (RowToList r rl, TsBridgeVariantEncodedNestedRL tok symTag symVal rl) => TsBridgeVariantEncodedNested tok symTag symVal r where
  tsBridgeVariantEncodedNested tok _ = DTS.TsTypeUnion <$> tsBridgeVariantEncodedNestedRL tok (Proxy :: _ symTag) (Proxy :: _ symVal) (Proxy :: _ rl)

-------------------------------------------------------------------------------
-- tsBridge methods / class TsBridgeVariantEncodedNestedRL
-------------------------------------------------------------------------------

class TsBridgeVariantEncodedNestedRL :: Type -> Symbol -> Symbol -> RowList Type -> Constraint
class TsBridgeVariantEncodedNestedRL tok symTag symVal rl where
  tsBridgeVariantEncodedNestedRL :: tok -> Proxy symTag -> Proxy symVal -> Proxy rl -> TsBridgeM (Array DTS.TsType)

instance TsBridgeVariantEncodedNestedRL tok symTag symVal Nil where
  tsBridgeVariantEncodedNestedRL _ _ _ _ = pure []

instance
  ( TsBridgeBy tok t
  , TsBridgeVariantEncodedNestedRL tok symTag symVal rl
  , IsSymbol s
  , IsSymbol symTag
  , IsSymbol symVal
  ) =>
  TsBridgeVariantEncodedNestedRL tok symTag symVal (Cons s t rl) where
  tsBridgeVariantEncodedNestedRL tok prxSymTag prxSymVal _ =
    do
      x <- tsBridgeBy tok (Proxy :: _ t)
      xs <- tsBridgeVariantEncodedNestedRL tok prxSymTag prxSymVal (Proxy :: _ rl)
      pure $
        A.cons
          ( DTS.TsTypeRecord
              [ DTS.TsRecordField (reflectSymbol prxSymTag)
                  { readonly: true, optional: false }
                  (DTS.TsTypeTypelevelString $ reflectSymbol (Proxy :: _ s))
              , DTS.TsRecordField (reflectSymbol prxSymVal)
                  { readonly: true, optional: false }
                  x
              ]
          )
          xs

-------------------------------------------------------------------------------
-- Util
-------------------------------------------------------------------------------

fixScope :: Scope -> Scope
fixScope (Scope { fixed, floating }) = Scope
  { floating: OSet.empty
  , fixed: fixed <> floating
  }

mkBrandedTypeDecl :: DTS.TsName -> OSet DTS.TsName -> Maybe DTS.TsType -> DTS.TsDeclaration
mkBrandedTypeDecl name args type_ = DTS.TsDeclTypeDef name DTS.Public (coerce args)
  $ maybeWithType
  $
    DTS.TsTypeRecord
      (opaqueField : (mapWithIndex mkArgFields $ OSet.toUnfoldable $ coerce args))

  where
  maybeWithType = case type_ of
    Nothing -> identity
    Just t -> \x -> DTS.TsTypeIntersection [ x, t ]

  opaqueField = DTS.TsRecordField
    "__brand"
    { optional: false, readonly: true }
    DTS.TsTypeUniqueSymbol

  mkArgFields idx name' = DTS.TsRecordField
    ("__arg" <> show (idx + 1))
    { optional: false, readonly: true }
    (DTS.TsTypeVar name')
