module TsBridge.DefaultImpls
  ( Var(..)
  , class DefaultRecord
  , class DefaultRecordRL
  , class DefaultVariant
  , class DefaultVariantRL
  , defaultArray
  , defaultBoolean
  , defaultBrandedType
  , defaultChar
  , defaultEffect
  , defaultEither
  , defaultFunction
  , defaultInt
  , defaultMaybe
  , defaultNullable
  , defaultNumber
  , defaultOpaqueType
  , defaultPromise
  , defaultRecord
  , defaultRecordRL
  , defaultString
  , defaultTuple
  , defaultTypeVar
  , defaultUnit
  , defaultVariant
  , defaultVariantRL
  ) where

import Prelude

import Control.Monad.Writer (censor, listen, tell)
import Control.Promise (Promise)
import Data.Array (mapWithIndex, (:))
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Data.Nullable (Nullable)
import Data.Set as Set
import Data.Set.Ordered (OSet)
import Data.Set.Ordered as OSet
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (sequence)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Undefined (undefined)
import Data.Variant (Variant)
import Effect (Effect)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Record as R
import Safe.Coerce (coerce)
import TsBridge.Core (class TsBridgeBy, StandaloneTsType, tsBridgeBy)
import TsBridge.DTS as DTS
import TsBridge.Monad (Scope(..), TsBridgeAccum(..), TsBridgeM)
import Type.Proxy (Proxy(..))

data Var (s :: Symbol) = Var

-- | Default type class method implementation for type variables.
-- | This is needed because polymorphic values cannot be exported directly.
-- | They have to be monomorphized: E.g. something of type `Maybe a` needs to be
-- | typed `Maybe (Var "A")` to be exported.
defaultTypeVar :: forall s. IsSymbol s => Var s -> StandaloneTsType
defaultTypeVar _ = do
  let
    tsName = DTS.TsName $ reflectSymbol (Proxy :: _ s)

    scope = Scope
      { floating: OSet.singleton tsName
      , fixed: OSet.empty
      }

  tell
    $ over TsBridgeAccum _ { scope = scope } mempty
  pure $ DTS.TsTypeVar tsName

-- defaultProxy :: forall f a. TsBridgeBy f a => f -> Proxy a -> StandaloneTsType
-- defaultProxy mp _ = tsBridgeBy mp (undefined :: a)

-- | Default type class method implementation for the `Number` type
-- | Generates a TypeScript `number` type
defaultNumber :: Proxy Number -> StandaloneTsType
defaultNumber _ = pure DTS.TsTypeNumber

-- | Default type class method implementation for the `String` type
-- | Generates a TypeScript `string` type
defaultString :: Proxy String -> StandaloneTsType
defaultString _ = pure DTS.TsTypeString

-- | Default type class method implementation for the `Boolean` type
-- | Generates a TypeScript `boolean` type
defaultBoolean :: Proxy Boolean -> StandaloneTsType
defaultBoolean _ = pure DTS.TsTypeBoolean

-- | Default type class method implementation for the `Int` type
-- | Generates a TypeScript opaque type
defaultInt :: Proxy Int -> StandaloneTsType
defaultInt = defaultOpaqueType "Prim" "Int" [] []

-- | Default type class method implementation for the `Char` type
-- | Generates a TypeScript opaque type
defaultChar :: Proxy Char -> StandaloneTsType
defaultChar = defaultOpaqueType "Prim" "Char" [] []

-- | Default type class method implementation for the `Unit` type
-- | Generates a TypeScript `void` type
defaultUnit :: Proxy Unit -> StandaloneTsType
defaultUnit _ = pure DTS.TsTypeVoid

-- | Default type class method implementation for the `Effect` type
-- | Generates a TypeScript `() => void` type
defaultEffect
  :: forall a tok
   . TsBridgeBy tok a
  => tok
  -> Proxy (Effect a)
  -> StandaloneTsType
defaultEffect tok _ = censor mapAccum ado
  ret /\ TsBridgeAccum { scope: Scope scopeRet } <- listen $ tsBridgeBy tok (Proxy :: _ a)
  let
    newFixed = (scopeRet.fixed)
      <> scopeRet.floating

    removeQuant =
      DTS.mapQuantifier $ OSet.filter (_ `OSet.notElem` newFixed)

  in
    DTS.TsTypeFunction (DTS.TsTypeArgsQuant $ coerce newFixed)
      [
      ]
      (removeQuant ret)
  where
  mapAccum = over TsBridgeAccum (\x -> x { scope = fixScope x.scope })

-- | Default type class method implementation for the `Array a` type
-- | Generates a TypeScript `Array<A>` type
defaultArray :: forall a f. TsBridgeBy f a => f -> Proxy (Array a) -> StandaloneTsType
defaultArray f _ = DTS.TsTypeArray <$> tsBridgeBy f (Proxy :: _ a)

-- | Default type class method implementation for the `Tuple a b` type
-- | Generates a TypeScript opaque type
defaultTuple
  :: forall tok a b
   . TsBridgeBy tok a
  => TsBridgeBy tok b
  => tok
  -> Proxy (Tuple a b)
  -> StandaloneTsType
defaultTuple tok =
  defaultOpaqueType "Data.Tuple" "Tuple" [ "A", "B" ]
    [ tsBridgeBy tok (Proxy :: _ a)
    , tsBridgeBy tok (Proxy :: _ b)
    ]

-- | Default type class method implementation for the `Either a b` type
-- | Generates a TypeScript opaque type
defaultEither
  :: forall tok a b
   . TsBridgeBy tok a
  => TsBridgeBy tok b
  => tok
  -> Proxy (Tuple a b)
  -> StandaloneTsType
defaultEither tok =
  defaultOpaqueType "Data.Either" "Either" [ "A", "B" ]
    [ tsBridgeBy tok (Proxy :: _ a)
    , tsBridgeBy tok (Proxy :: _ b)
    ]

-- | Default type class method implementation for the `Maybe a` type
-- | Generates a TypeScript opaque type
defaultMaybe
  :: forall tok a
   . TsBridgeBy tok a
  => tok
  -> Proxy (Maybe a)
  -> StandaloneTsType
defaultMaybe tok =
  defaultOpaqueType "Data.Maybe" "Maybe" [ "A" ]
    [ tsBridgeBy tok (Proxy :: _ a) ]

-- | Default type class method implementation for the `Promise a` type
-- | Generates a TypeScript `Promise<A>` type
defaultPromise
  :: forall a f
   . TsBridgeBy f a
  => f
  -> Proxy (Promise a)
  -> StandaloneTsType
defaultPromise f _ = do
  x <- tsBridgeBy f (Proxy :: _ a)
  pure $ DTS.TsTypeConstructor
    (DTS.TsQualName Nothing (DTS.TsName "Promise"))
    (DTS.TsTypeArgs [ x ])

-- | Default type class method implementation for the `Nullable a` type
-- | Generates a TypeScript `A | null` type
defaultNullable
  :: forall a tok
   . TsBridgeBy tok a
  => tok
  -> Proxy (Nullable a)
  -> StandaloneTsType
defaultNullable tok _ = do
  x <- tsBridgeBy tok (Proxy :: _ a)
  pure $ DTS.TsTypeUnion
    [ DTS.TsTypeNull, x ]

-- | Default type class method implementation for the `a -> b` (Function) type
-- | Generates a TypeScript `(_ : A) => B` type
defaultFunction
  :: forall f a b
   . TsBridgeBy f a
  => TsBridgeBy f b
  => f
  -> Proxy (a -> b)
  -> StandaloneTsType
defaultFunction f _ = censor mapAccum ado
  arg /\ TsBridgeAccum { scope: Scope scopeArg } <- listen $ tsBridgeBy f (Proxy :: _ a)
  ret /\ TsBridgeAccum { scope: Scope scopeRet } <- listen $ tsBridgeBy f (Proxy :: _ b)
  let
    newFixed = (OSet.intersect scopeArg.fixed scopeRet.fixed)
      <> scopeArg.floating
      <> scopeRet.floating

    removeQuant =
      DTS.mapQuantifier $ OSet.filter (_ `OSet.notElem` newFixed)

  in
    DTS.TsTypeFunction (DTS.TsTypeArgsQuant $ coerce newFixed)
      [ DTS.TsFnArg (DTS.TsName "_") (removeQuant arg)
      ]
      (removeQuant ret)
  where
  mapAccum = over TsBridgeAccum (\x -> x { scope = fixScope x.scope })

-------------------------------------------------------------------------------
-- Class / DefaultRecord
-------------------------------------------------------------------------------

class DefaultRecord :: Type -> Row Type -> Constraint
class DefaultRecord mp r where
  defaultRecord :: mp -> Proxy (Record r) -> StandaloneTsType

instance (RowToList r rl, DefaultRecordRL mp rl) => DefaultRecord mp r where
  defaultRecord mp _ = DTS.TsTypeRecord <$> defaultRecordRL mp (Proxy :: _ rl)

-------------------------------------------------------------------------------
-- Class / DefaultRecordRL
-------------------------------------------------------------------------------

class DefaultRecordRL :: Type -> RowList Type -> Constraint
class DefaultRecordRL mp rl where
  defaultRecordRL :: mp -> Proxy rl -> TsBridgeM (Array DTS.TsRecordField)

instance DefaultRecordRL mp Nil where
  defaultRecordRL _ _ = pure []

instance
  ( TsBridgeBy mp t
  , DefaultRecordRL mp rl
  , IsSymbol s
  ) =>
  DefaultRecordRL mp (Cons s t rl) where
  defaultRecordRL mp _ = do
    x <- tsBridgeBy mp (Proxy :: _ t)
    xs <- defaultRecordRL mp (Proxy :: _ rl)
    let k = DTS.TsName $ reflectSymbol (Proxy :: _ s)
    pure $
      A.cons (DTS.TsRecordField k { optional: false, readonly: true } x) xs

-------------------------------------------------------------------------------
-- Class / DefaultVariant
-------------------------------------------------------------------------------

class DefaultVariant :: Type -> Row Type -> Constraint
class DefaultVariant mp r where
  defaultVariant :: mp -> Proxy (Variant r) -> StandaloneTsType

instance (RowToList r rl, DefaultVariantRL mp rl) => DefaultVariant mp r where
  defaultVariant mp _ = DTS.TsTypeUnion <$> defaultVariantRL mp (Proxy :: _ rl)

-------------------------------------------------------------------------------
-- Class / DefaultVariantRL
-------------------------------------------------------------------------------

class DefaultVariantRL :: Type -> RowList Type -> Constraint
class DefaultVariantRL mp rl where
  defaultVariantRL :: mp -> Proxy rl -> TsBridgeM (Array DTS.TsType)

instance DefaultVariantRL mp Nil where
  defaultVariantRL _ _ = pure []

instance
  ( TsBridgeBy mp t
  , DefaultVariantRL mp rl
  , IsSymbol s
  ) =>
  DefaultVariantRL mp (Cons s t rl) where
  defaultVariantRL mp _ =
    do
      x <- tsBridgeBy mp (Proxy :: _ t)
      xs <- defaultVariantRL mp (Proxy :: _ rl)
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

defaultOpaqueType :: forall a. String -> String -> Array String -> Array (StandaloneTsType) -> a -> StandaloneTsType
defaultOpaqueType pursModuleName pursTypeName targNames targs _ = brandedType
  (DTS.TsFilePath (pursModuleName <> "/index") "d.ts")
  (DTS.TsModuleAlias pursModuleName)
  (DTS.TsName pursTypeName)
  (OSet.fromFoldable $ DTS.TsName <$> targNames)
  targs
  Nothing

defaultBrandedType
  :: forall mp a t
   . Newtype a t
  => TsBridgeBy mp t
  => mp
  -> String
  -> String
  -> Array String
  -> Array (StandaloneTsType)
  -> Proxy a
  -> StandaloneTsType
defaultBrandedType mp pursModuleName pursTypeName targNames targs t = do
  x <- tsBridgeBy mp (Proxy :: _ t)
  brandedType
    (DTS.TsFilePath (pursModuleName <> "/index") "d.ts")
    (DTS.TsModuleAlias pursModuleName)
    (DTS.TsName pursTypeName)
    (OSet.fromFoldable $ DTS.TsName <$> targNames)
    targs
    (Just x)

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
      (opaqueField : (mapWithIndex mkArgFields $ OSet.toUnfoldable args))

  where
  maybeWithType = case type_ of
    Nothing -> identity
    Just t -> \x -> DTS.TsTypeIntersection [ x, t ]

  opaqueField = DTS.TsRecordField
    (DTS.TsName $ "__brand")
    { optional: false, readonly: true }
    DTS.TsTypeUniqueSymbol

  mkArgFields idx name' = DTS.TsRecordField
    (DTS.TsName ("__arg" <> show idx))
    { optional: false, readonly: true }
    (DTS.TsTypeVar name')
