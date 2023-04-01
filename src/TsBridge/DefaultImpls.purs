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
import DTS (OSet(..))
import DTS as DTS
import Data.Array (mapWithIndex, (:))
import Data.Array as A
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over)
import Data.Nullable (Nullable)
import Data.Set.Ordered as OSet
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Effect (Effect)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Record as R
import Safe.Coerce (coerce)
import TsBridge.Core (class TsBridgeBy, tsBridgeBy)
import TsBridge.Monad (Scope(..), TsBridgeAccum(..), TsBridgeM)
import TsBridge.Types (mkName, Name, DefName(..), unsafeName, toTsName)
import Type.Proxy (Proxy(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Represents a monomorphized type variable. E.g. a `Maybe a` can become a
-- | `Maybe (TypeVar "A")`. It's useful to create some type aliases for
-- | variables that are used often, like: `type A = TypeVar "A"`.
data TypeVar (s :: Symbol) = TypeVar

-------------------------------------------------------------------------------
-- tsBridge methods
-------------------------------------------------------------------------------

-- | `tsBridge` type class method implementation for type variables.
-- | This is needed because polymorphic values cannot be exported directly.
-- | They have to be monomorphized: E.g. something of type `Maybe a` needs to be
-- | typed `Maybe (Var "A")` to be exported.
tsBridgeTypeVar :: forall s. IsSymbol s => Proxy (TypeVar s) -> TsBridgeM DTS.TsType
tsBridgeTypeVar _ = do

  name <- mkName $ DefName $ reflectSymbol (Proxy :: _ s)

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
tsBridgeInt = tsBridgeOpaqueType "Prim" (DefName "Int") []

-- | `tsBridge` type class method implementation for the `Char` type
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#char)
-- | for details.
tsBridgeChar :: Proxy Char -> TsBridgeM DTS.TsType
tsBridgeChar = tsBridgeOpaqueType "Prim" (DefName "Char") []

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
  tsBridgeOpaqueType "Data.Tuple" (DefName "Tuple")
    [ DefName "A" /\ tsBridgeBy tok (Proxy :: _ a)
    , DefName "B" /\ tsBridgeBy tok (Proxy :: _ b)
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
  -> TsBridgeM DTS.TsType
tsBridgeEither tok =
  tsBridgeOpaqueType "Data.Either" (DefName "Either")
    [ DefName "A" /\ tsBridgeBy tok (Proxy :: _ a)
    , DefName "B" /\ tsBridgeBy tok (Proxy :: _ b)
    ]

-- | `tsBridge` type class method implementation for the `Maybe` type
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#maybe)
-- | for details.
tsBridgeMaybe :: forall tok a. TsBridgeBy tok a => tok -> Proxy (Maybe a) -> TsBridgeM DTS.TsType
tsBridgeMaybe tok =
  tsBridgeOpaqueType "Data.Maybe" (DefName "Maybe")
    [ DefName "A" /\ tsBridgeBy tok (Proxy :: _ a) ]

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

  in
    DTS.TsTypeFunction (DTS.TsTypeArgsQuant $ coerce newFixed)
      [ DTS.TsFnArg (DTS.TsName "_") (removeQuant arg)
      ]
      (removeQuant ret)
  where
  mapAccum = over TsBridgeAccum (\x -> x { scope = fixScope x.scope })

-- | `tsBridge` type class method implementation for opaque types
tsBridgeOpaqueType :: forall a. String -> DefName -> Array (DefName /\ TsBridgeM DTS.TsType) -> a -> TsBridgeM DTS.TsType
tsBridgeOpaqueType pursModuleName pursTypeName args _ = do
  argNames <- args <#> fst # traverse mkName
  targs <- args <#> snd # sequence
  name <- mkName pursTypeName

  let
    filePath = DTS.TsFilePath (pursModuleName <> "/index.d.ts")
    importPath = DTS.TsImportPath ("../" <> pursModuleName)

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

-- | `tsBridge` type class method implementation for newtypes
tsBridgeNewtype
  :: forall tok a t
   . Newtype a t
  => TsBridgeBy tok t
  => tok
  -> String
  -> DefName
  -> Array (DefName /\ TsBridgeM DTS.TsType)
  -> Proxy a
  -> TsBridgeM DTS.TsType
tsBridgeNewtype tok pursModuleName pursTypeName args_ _ = do
  let
    targNames = map fst args_
    targs = map snd args_

  args <- sequence targs
  x <- tsBridgeBy tok (Proxy :: _ t)
  let
    filePath = DTS.TsFilePath (pursModuleName <> "/index.d.ts")
    filePathRef = DTS.TsImportPath ("../" <> pursModuleName)

  name <- mkName pursTypeName
  args' <- OSet.fromFoldable <$> map toTsName <$> traverse mkName targNames

  let
    typeDefs =
      [ DTS.TsModuleFile
          filePath
          ( DTS.TsModule
              [ DTS.TsDeclTypeDef (toTsName name) DTS.Public (coerce args') x
              ]
          )
      ]

  tell
    $ TsBridgeAccum
    $ R.union mempty { typeDefs }

  pure
    $ DTS.TsTypeConstructor (DTS.TsQualName (Just filePathRef) (toTsName name)) (DTS.TsTypeArgs args)

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
-- CltsBridge methods / class TsBridgeVariantRL
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
