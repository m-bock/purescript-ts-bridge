module TsBridge.DefaultImpls
  ( class DefaultRecord
  , class DefaultRecordRL
  , class DefaultVariant
  , class DefaultVariantRL
  , defaultArray
  , defaultBoolean
  , defaultBrandedType
  , defaultChar
  , defaultEffect
  , defaultFunction
  , defaultInt
  , defaultNullable
  , defaultNumber
  , defaultOpaqueType
  , defaultPromise
  , defaultProxy
  , defaultRecord
  , defaultRecordRL
  , defaultString
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
import Data.Newtype (class Newtype, over, over2, unwrap, wrap)
import Data.Nullable (Nullable)
import Data.Set as Set
import Data.Set.Ordered (OSet)
import Data.Set.Ordered as OSet
import Data.String (Pattern(..), Replacement(..))
import Data.String as Str
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Undefined (undefined)
import Data.Variant (Variant)
import Effect (Effect)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Record as R
import Safe.Coerce (coerce)
import TsBridge.Core (class TsBridgeBy, tsBridgeBy)
import TsBridge.DTS as DTS
import TsBridge.Monad (Scope, TsBridgeAccum(..), TsBridgeM, TsBridge_Monad_Wrap(..), defaultTsBridgeAccum)
import TsBridge.TypeVars (Var)
import Type.Proxy (Proxy(..))

-- | Default type class method implementation for type variables.
-- | This is needed because polymorphic values cannot be exported directly.
-- | They have to be monomorphized: E.g. something of type `Maybe a` needs to be
-- | typed `Maybe (Var "A")` to be exported.
defaultTypeVar :: forall s. IsSymbol s => Var s -> TsBridgeM DTS.TsType
defaultTypeVar _ = do
  let
    tsName = DTS.TsName $ reflectSymbol (Proxy :: _ s)

    scope =
      { floating: wrap $ OSet.singleton tsName
      , fixed: mempty
      }

  tell
    $ over TsBridgeAccum _ { scope = scope } defaultTsBridgeAccum
  pure $ DTS.TsTypeVar tsName

defaultProxy :: forall f a. TsBridgeBy f a => f -> Proxy a -> TsBridgeM DTS.TsType
defaultProxy mp _ = tsBridgeBy mp (undefined :: a)

-- | Default type class method implementation for the `Number` type
-- | Generates a TypeScript `number` type
defaultNumber :: Number -> TsBridgeM DTS.TsType
defaultNumber _ = pure DTS.TsTypeNumber

-- | Default type class method implementation for the `String` type
-- | Generates a TypeScript `string` type
defaultString :: String -> TsBridgeM DTS.TsType
defaultString _ = pure DTS.TsTypeString

-- | Default type class method implementation for the `Boolean` type
-- | Generates a TypeScript `boolean` type
defaultBoolean :: Boolean -> TsBridgeM DTS.TsType
defaultBoolean _ = pure DTS.TsTypeBoolean

-- | Default type class method implementation for the `Int` type
-- | Generates a TypeScript opaque type
defaultInt :: Int -> TsBridgeM DTS.TsType
defaultInt = defaultOpaqueType "Prim" "Int" [] []

-- | Default type class method implementation for the `Char` type
-- | Generates a TypeScript opaque type
defaultChar :: Char -> TsBridgeM DTS.TsType
defaultChar = defaultOpaqueType "Prim" "Char" [] []

-- | Default type class method implementation for the `Unit` type
-- | Generates a TypeScript `void` type
defaultUnit :: Unit -> TsBridgeM DTS.TsType
defaultUnit _ = pure DTS.TsTypeVoid

-- | Default type class method implementation for the `Effect` type
-- | Generates a TypeScript `() => void` type
defaultEffect
  :: forall a tok
   . TsBridgeBy tok (Proxy a)
  => tok
  -> Effect a
  -> TsBridgeM DTS.TsType
defaultEffect tok _ = censor mapAccum ado
  ret /\ TsBridgeAccum { scope: scopeRet } <- listen $ tsBridgeBy tok (Proxy :: _ a)
  let
    newFixed = (scopeRet.fixed)
      <> scopeRet.floating

    removeQuant =
      DTS.mapQuantifier $ OSet.filter (_ `OSet.notElem` (unwrap newFixed))

  in
    DTS.TsTypeFunction (DTS.TsTypeArgsQuant $ coerce newFixed)
      [
      ]
      (removeQuant ret)
  where
  mapAccum = over TsBridgeAccum (\x -> x { scope = fixScope x.scope })

-- | Default type class method implementation for the `Array a` type
-- | Generates a TypeScript `Array<A>` type
defaultArray
  :: forall a f
   . TsBridgeBy f (Proxy a)
  => f
  -> Array a
  -> TsBridgeM DTS.TsType
defaultArray f _ = DTS.TsTypeArray <$> tsBridgeBy f (Proxy :: _ a)

-- | Default type class method implementation for the `Promise a` type
-- | Generates a TypeScript `Promise<A>` type
defaultPromise
  :: forall a f
   . TsBridgeBy f (Proxy a)
  => f
  -> Promise a
  -> TsBridgeM DTS.TsType
defaultPromise f _ = do
  x <- tsBridgeBy f (Proxy :: _ a)
  pure $ DTS.TsTypeConstructor
    (DTS.TsQualName Nothing (DTS.TsName "Promise"))
    (DTS.TsTypeArgs [ x ])

-- | Default type class method implementation for the `Nullable a` type
-- | Generates a TypeScript `A | null` type
defaultNullable
  :: forall a tok
   . TsBridgeBy tok (Proxy a)
  => tok
  -> Nullable a
  -> TsBridgeM DTS.TsType
defaultNullable tok _ = do
  x <- tsBridgeBy tok (Proxy :: _ a)
  pure $ DTS.TsTypeUnion
    [ DTS.TsTypeNull, x ]

-- | Default type class method implementation for the `a -> b` (Function) type
-- | Generates a TypeScript `(_ : A) => B` type
defaultFunction
  :: forall f a b
   . TsBridgeBy f (Proxy a)
  => TsBridgeBy f (Proxy b)
  => f
  -> (a -> b)
  -> TsBridgeM DTS.TsType
defaultFunction f _ = censor mapAccum ado
  arg /\ TsBridgeAccum { scope: scopeArg } <- listen $ tsBridgeBy f (Proxy :: _ a)
  ret /\ TsBridgeAccum { scope: scopeRet } <- listen $ tsBridgeBy f (Proxy :: _ b)
  let
    newFixed = (over2 wrap OSet.intersect scopeArg.fixed scopeRet.fixed)
      <> scopeArg.floating
      <> scopeRet.floating

    removeQuant =
      DTS.mapQuantifier $ OSet.filter (_ `OSet.notElem` (unwrap newFixed))

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
  defaultRecord :: mp -> Record r -> TsBridgeM DTS.TsType

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
  ( TsBridgeBy mp (Proxy t)
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
  defaultVariant :: mp -> Variant r -> TsBridgeM DTS.TsType

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
  ( TsBridgeBy mp (Proxy t)
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

defaultOpaqueType :: forall a. String -> String -> Array String -> Array (TsBridgeM DTS.TsType) -> a -> TsBridgeM DTS.TsType
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
  -> Array (TsBridgeM DTS.TsType)
  -> a
  -> TsBridgeM DTS.TsType
defaultBrandedType mp pursModuleName pursTypeName targNames targs t = do
  x <- tsBridgeBy mp $ unwrap t
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
fixScope { fixed, floating } =
  { floating: mempty
  , fixed: fixed <> floating
  }

brandedType :: DTS.TsFilePath -> DTS.TsModuleAlias -> DTS.TsName -> OSet DTS.TsName -> Array (TsBridgeM DTS.TsType) -> Maybe DTS.TsType -> TsBridgeM DTS.TsType
brandedType filePath moduleAlias name targs args' type_ = do
  args <- sequence args'

  let
    typeDefs =
      [ DTS.TsModuleFile
          filePath
          ( DTS.TsModule Set.empty
              [ mkBrandedTypeDecl name targs type_
              ]
          )
      ]

  tell
    $ TsBridgeAccum
    $ R.union mempty { typeDefs }

  pure
    $ DTS.TsTypeConstructor (DTS.TsQualName (Just moduleAlias) name) (DTS.TsTypeArgs args)

dotsToLodashes :: String -> String
dotsToLodashes = Str.replaceAll (Pattern ".") (Replacement "_")

filePathToModulePath :: DTS.TsFilePath -> DTS.TsModulePath
filePathToModulePath (DTS.TsFilePath x _) = DTS.TsModulePath x

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
    (DTS.TsName $ "brand")
    { optional: false, readonly: true }
    DTS.TsTypeUniqueSymbol

  mkArgFields idx name' = DTS.TsRecordField
    (DTS.TsName ("arg" <> show idx))
    { optional: false, readonly: true }
    (DTS.TsTypeVar name')
