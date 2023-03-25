module TsBridge.DefaultImpls
  ( TypeVar(..)
  , class TsBridgeRecord
  , class TsBridgeRecordRL
  , class TsBridgeVariant
  , class TsBridgeVariantRL
  , tsBridgeArray
  , tsBridgeBoolean
  , tsBridgeNewtype
  , tsBridgeChar
  , tsBridgeEffect
  , tsBridgeEither
  , tsBridgeFunction
  , tsBridgeInt
  , tsBridgeMaybe
  , tsBridgeNullable
  , tsBridgeNumber
  , tsBridgeOpaqueType
  , tsBridgePromise
  , tsBridgeRecord
  , tsBridgeRecordRL
  , tsBridgeString
  , tsBridgeTuple
  , tsBridgeTypeVar
  , tsBridgeUnit
  , tsBridgeVariant
  , tsBridgeVariantRL
  ) where

import Prelude

import Control.Monad.Writer (censor, listen, tell)
import Control.Promise (Promise)
import Data.Array (mapWithIndex, (:))
import Data.Array as A
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over)
import Data.Nullable (Nullable)
import Data.Set as Set
import Data.Set.Ordered as OSet
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (sequence)
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Effect (Effect)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Record as R
import Safe.Coerce (coerce)
import TsBridge.Core (class TsBridgeBy, StandaloneTsType, tsBridgeBy)
import TsBridge.DTS (OSet(..))
import TsBridge.DTS as DTS
import TsBridge.Monad (Scope(..), TsBridgeAccum(..), TsBridgeM)
import Type.Proxy (Proxy(..))

-- | Represents a monomorphized type variable. E.g. a `Maybe a` can become a
-- | `Maybe (TypeVar "A")`. It's useful to create some type aliases for
-- | variables that are used often, like: `type A = TypeVar "A"`.
data TypeVar (s :: Symbol) = TypeVar

-- | `tsBridge` type class method implementation for type variables.
-- | This is needed because polymorphic values cannot be exported directly.
-- | They have to be monomorphized: E.g. something of type `Maybe a` needs to be
-- | typed `Maybe (Var "A")` to be exported.
tsBridgeTypeVar :: forall s. IsSymbol s => Proxy (TypeVar s) -> StandaloneTsType
tsBridgeTypeVar _ = do
  let
    tsName = DTS.TsName $ reflectSymbol (Proxy :: _ s)

    scope = Scope
      { floating: coerce $ OSet.singleton tsName
      , fixed: OSet.empty
      }

  tell
    $ over TsBridgeAccum _ { scope = scope } mempty
  pure $ DTS.TsTypeVar tsName

-- | `tsBridge` type class method implementation for the `Number` type.
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#number)
-- | for details.
tsBridgeNumber :: Proxy Number -> StandaloneTsType
tsBridgeNumber _ = pure DTS.TsTypeNumber

-- | `tsBridge` type class method implementation for the `String` type
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#string)
-- | for details.
tsBridgeString :: Proxy String -> StandaloneTsType
tsBridgeString _ = pure DTS.TsTypeString

-- | `tsBridge` type class method implementation for the `Boolean` type
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#boolean)
-- | for details.
tsBridgeBoolean :: Proxy Boolean -> StandaloneTsType
tsBridgeBoolean _ = pure DTS.TsTypeBoolean

-- | `tsBridge` type class method implementation for the `Int` type
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#int)
-- | for details.
tsBridgeInt :: Proxy Int -> StandaloneTsType
tsBridgeInt = tsBridgeOpaqueType "Prim" "Int" []

-- | `tsBridge` type class method implementation for the `Char` type
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#char)
-- | for details.
tsBridgeChar :: Proxy Char -> StandaloneTsType
tsBridgeChar = tsBridgeOpaqueType "Prim" "Char" []

-- | `tsBridge` type class method implementation for the `Unit` type
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#unit)
-- | for details.
tsBridgeUnit :: Proxy Unit -> StandaloneTsType
tsBridgeUnit _ = pure DTS.TsTypeVoid

-- | `tsBridge` type class method implementation for the `Effect` type
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#number)
-- | for details.
tsBridgeEffect :: forall tok a. TsBridgeBy tok a => tok -> Proxy (Effect a) -> StandaloneTsType
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
tsBridgeArray :: forall a tok. TsBridgeBy tok a => tok -> Proxy (Array a) -> StandaloneTsType
tsBridgeArray tok _ = DTS.TsTypeArray <$> tsBridgeBy tok (Proxy :: _ a)

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
  -> StandaloneTsType
tsBridgeTuple tok =
  tsBridgeOpaqueType "Data.Tuple" "Tuple"
    [ "A" /\ tsBridgeBy tok (Proxy :: _ a)
    , "B" /\ tsBridgeBy tok (Proxy :: _ b)
    ]

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
  -> StandaloneTsType
tsBridgeEither tok =
  tsBridgeOpaqueType "Data.Either" "Either"
    [ "A" /\ tsBridgeBy tok (Proxy :: _ a)
    , "B" /\ tsBridgeBy tok (Proxy :: _ b)
    ]

-- | `tsBridge` type class method implementation for the `Maybe` type
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#maybe)
-- | for details.
tsBridgeMaybe :: forall tok a. TsBridgeBy tok a => tok -> Proxy (Maybe a) -> StandaloneTsType
tsBridgeMaybe tok =
  tsBridgeOpaqueType "Data.Maybe" "Maybe"
    [ "A" /\ tsBridgeBy tok (Proxy :: _ a) ]

-- | `tsBridge` type class method implementation for the `Promise` type.
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#promise)
-- | for details.
tsBridgePromise :: forall tok a. TsBridgeBy tok a => tok -> Proxy (Promise a) -> StandaloneTsType
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
tsBridgeNullable :: forall a tok. TsBridgeBy tok a => tok -> Proxy (Nullable a) -> StandaloneTsType
tsBridgeNullable tok _ = do
  x <- tsBridgeBy tok (Proxy :: _ a)
  pure $ DTS.TsTypeUnion
    [ DTS.TsTypeNull, x ]

-- | `tsBridge` type class method implementation for the `a -> b` (Function) type
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#function)
-- | for details.
tsBridgeFunction
  :: forall tok a b
   . TsBridgeBy tok a
  => TsBridgeBy tok b
  => tok
  -> Proxy (a -> b)
  -> StandaloneTsType
tsBridgeFunction tok _ = censor mapAccum ado
  arg /\ TsBridgeAccum { scope: Scope scopeArg } <- listen $ tsBridgeBy tok (Proxy :: _ a)
  ret /\ TsBridgeAccum { scope: Scope scopeRet } <- listen $ tsBridgeBy tok (Proxy :: _ b)
  let
    newFixed = (OSet.intersect scopeArg.fixed scopeRet.fixed)
      <> scopeArg.floating
      <> scopeRet.floating

    removeQuant =
      DTS.mapQuantifier $ coerce $ OSet.filter (_ `OSet.notElem` newFixed)

  in
    DTS.TsTypeFunction (DTS.TsTypeArgsQuant $ coerce newFixed)
      [ DTS.TsFnArg (DTS.TsName "_") (removeQuant arg)
      ]
      (removeQuant ret)
  where
  mapAccum = over TsBridgeAccum (\x -> x { scope = fixScope x.scope })

-------------------------------------------------------------------------------
-- Class / tsBridgeRecord
-------------------------------------------------------------------------------

class TsBridgeRecord :: Type -> Row Type -> Constraint
class TsBridgeRecord tok r where
  -- | `tsBridge` type class method implementation for the Record type
  -- |
  -- | See [this
  -- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#record)
  -- | for details.
  tsBridgeRecord :: tok -> Proxy (Record r) -> StandaloneTsType

instance (RowToList r rl, TsBridgeRecordRL tok rl) => TsBridgeRecord tok r where
  tsBridgeRecord tok _ = DTS.TsTypeRecord <$> tsBridgeRecordRL tok (Proxy :: _ rl)

-------------------------------------------------------------------------------
-- Class / tsBridgeRecordRL
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
    let k = DTS.TsName $ reflectSymbol (Proxy :: _ s)
    pure $
      A.cons (DTS.TsRecordField k { optional: false, readonly: true } x) xs

-------------------------------------------------------------------------------
-- Class / tsBridgeVariant
-------------------------------------------------------------------------------

class TsBridgeVariant :: Type -> Row Type -> Constraint
class TsBridgeVariant tok r where
  -- | `tsBridge` type class method implementation for the Variant type
  -- |
  -- | See [this
  -- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#variant)
  -- | for details.
  tsBridgeVariant :: tok -> Proxy (Variant r) -> StandaloneTsType

instance (RowToList r rl, TsBridgeVariantRL tok rl) => TsBridgeVariant tok r where
  tsBridgeVariant tok _ = DTS.TsTypeUnion <$> tsBridgeVariantRL tok (Proxy :: _ rl)

-------------------------------------------------------------------------------
-- Class / tsBridgeVariantRL
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
              [ DTS.TsRecordField (DTS.TsName "type")
                  { readonly: true, optional: false }
                  (DTS.TsTypeTypelevelString $ reflectSymbol (Proxy :: _ s))
              , DTS.TsRecordField (DTS.TsName "value")
                  { readonly: true, optional: false }
                  x
              ]
          )
          xs

-------------------------------------------------------------------------------

-- | `tsBridge` type class method implementation for opaque types
tsBridgeOpaqueType :: forall a. String -> String -> Array (String /\ StandaloneTsType) -> a -> StandaloneTsType
tsBridgeOpaqueType pursModuleName pursTypeName args _ = brandedType
  (DTS.TsFilePath (pursModuleName <> "/index") "d.ts")
  (DTS.TsModuleAlias pursModuleName)
  (DTS.TsName pursTypeName)
  (coerce $ OSet.fromFoldable $ DTS.TsName <$> targNames)
  targs
  Nothing
  where
  targNames = map fst args
  targs = map snd args

-- | `tsBridge` type class method implementation for newtypes
tsBridgeNewtype
  :: forall tok a t
   . Newtype a t
  => TsBridgeBy tok t
  => tok
  -> String
  -> String
  -> Array (String /\ StandaloneTsType)
  -> Proxy a
  -> StandaloneTsType
tsBridgeNewtype tok pursModuleName pursTypeName args_ _ = do
  let
    targNames = map fst args_
    targs = map snd args_

  args <- sequence targs
  x <- tsBridgeBy tok (Proxy :: _ t)
  let
    filePath = DTS.TsFilePath (pursModuleName <> "/index") "d.ts"
    alias = DTS.TsModuleAlias pursModuleName
    name = DTS.TsName pursTypeName
    args' = OSet.fromFoldable $ DTS.TsName <$> targNames

  let
    typeDefs =
      [ DTS.TsModuleFile
          filePath
          ( DTS.TsModule pursModuleName Set.empty
              [ DTS.TsDeclTypeDef name DTS.Public (coerce args') x
              ]
          )
      ]

  tell
    $ TsBridgeAccum
    $ R.union mempty { typeDefs }

  pure
    $ DTS.TsTypeConstructor (DTS.TsQualName (Just alias) name) (DTS.TsTypeArgs args)

-------------------------------------------------------------------------------
-- Util
-------------------------------------------------------------------------------

fixScope :: Scope -> Scope
fixScope (Scope { fixed, floating }) = Scope
  { floating: OSet.empty
  , fixed: fixed <> floating
  }

brandedType :: DTS.TsFilePath -> DTS.TsModuleAlias -> DTS.TsName -> OSet DTS.TsName -> Array (StandaloneTsType) -> Maybe DTS.TsType -> StandaloneTsType
brandedType filePath moduleAlias@(DTS.TsModuleAlias alias) name targs args' type_ = do
  args <- sequence args'

  let
    typeDefs =
      [ DTS.TsModuleFile
          filePath
          ( DTS.TsModule alias Set.empty
              [ mkBrandedTypeDecl name targs type_
              ]
          )
      ]

  tell
    $ TsBridgeAccum
    $ R.union mempty { typeDefs }

  pure
    $ DTS.TsTypeConstructor (DTS.TsQualName (Just moduleAlias) name) (DTS.TsTypeArgs args)

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
    (DTS.TsName $ "__brand")
    { optional: false, readonly: true }
    DTS.TsTypeUniqueSymbol

  mkArgFields idx name' = DTS.TsRecordField
    (DTS.TsName ("__arg" <> show (idx + 1)))
    { optional: false, readonly: true }
    (DTS.TsTypeVar name')
