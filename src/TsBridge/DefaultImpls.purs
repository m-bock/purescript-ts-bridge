module TsBridge.DefaultImpls
  ( class DefaultRecord
  , class DefaultRecordRL
  , defaultArray
  , defaultBoolean
  , defaultBrandedType
  , defaultEffect
  , defaultFunction
  , defaultNumber
  , defaultOpaqueType
  , defaultPromise
  , defaultProxy
  , defaultRecord
  , defaultString
  , defaultTypeVar
  , defaultUnit
  , defaultRecordRL
  ) where

import Prelude

import Control.Monad.Writer (censor, listen, tell)
import Control.Promise (Promise)
import Data.Array (mapWithIndex, (:))
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, over2, unwrap, wrap)
import Data.Set as Set
import Data.Set.Ordered (OSet)
import Data.Set.Ordered as OSet
import Data.Set.Ordered as Oset
import Data.String (Pattern(..), Replacement(..))
import Data.String as Str
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Undefined (undefined)
import Effect (Effect)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Record as R
import Safe.Coerce (coerce)
import TsBridge.DTS (TsBridge_DTS_Wrap(..), TsDeclVisibility(..), TsDeclaration(..), TsFilePath(..), TsFnArg(..), TsImport(..), TsModule(..), TsModuleAlias(..), TsModuleFile(..), TsModulePath(..), TsName(..), TsQualName(..), TsRecordField(..), TsType(..), TsTypeArgs(..), TsTypeArgsQuant(..), mapQuantifier)
import TsBridge.Monad (Scope, TsBridgeAccum(..), TsBridgeM, TsBridge_Monad_Wrap(..), defaultTsBridgeAccum)
import TsBridge.TypeVars (Var)
import Type.Proxy (Proxy(..))
import TsBridge.Core (class ToTsBridgeBy, toTsBridgeBy)

defaultTypeVar :: forall s. IsSymbol s => Var s -> TsBridgeM TsType
defaultTypeVar _ = do
  let
    tsName = TsName $ reflectSymbol (Proxy :: _ s)

    scope =
      { floating: wrap $ OSet.singleton tsName
      , fixed: mempty
      }

  tell
    $ over TsBridgeAccum _ { scope = scope } defaultTsBridgeAccum
  pure $ TsTypeVar tsName

defaultProxy :: forall f a. ToTsBridgeBy f a => f -> Proxy a -> TsBridgeM TsType
defaultProxy mp _ = toTsBridgeBy mp (undefined :: a)

defaultNumber :: Number -> TsBridgeM TsType
defaultNumber _ = pure TsTypeNumber

defaultString :: String -> TsBridgeM TsType
defaultString _ = pure TsTypeString

defaultBoolean :: Boolean -> TsBridgeM TsType
defaultBoolean _ = pure TsTypeBoolean

defaultUnit :: Unit -> TsBridgeM TsType
defaultUnit _ = pure TsTypeVoid

defaultEffect
  :: forall a f
   . ToTsBridgeBy f (Proxy a)
  => f
  -> Effect a
  -> TsBridgeM TsType
defaultEffect f _ = do
  x <- (toTsBridgeBy f (Proxy :: _ a))
  pure $ TsTypeFunction
    (TsTypeArgsQuant $ coerce $ Oset.singleton $ TsName "A")
    []
    x

defaultArray
  :: forall a f
   . ToTsBridgeBy f (Proxy a)
  => f
  -> Array a
  -> TsBridgeM TsType
defaultArray f _ = TsTypeArray <$> toTsBridgeBy f (Proxy :: _ a)

defaultPromise
  :: forall a f
   . ToTsBridgeBy f (Proxy a)
  => f
  -> Promise a
  -> TsBridgeM TsType
defaultPromise f _ = do
  x <- toTsBridgeBy f (Proxy :: _ a)
  pure $ TsTypeConstructor
    (TsQualName Nothing (TsName "Promise"))
    (TsTypeArgs [ x ])

defaultFunction
  :: forall f a b
   . ToTsBridgeBy f (Proxy a)
  => ToTsBridgeBy f (Proxy b)
  => f
  -> (a -> b)
  -> TsBridgeM TsType
defaultFunction f _ = censor mapAccum ado
  arg /\ TsBridgeAccum { scope: scopeArg } <- listen $ toTsBridgeBy f (Proxy :: _ a)
  ret /\ TsBridgeAccum { scope: scopeRet } <- listen $ toTsBridgeBy f (Proxy :: _ b)
  let
    newFixed = (over2 wrap OSet.intersect scopeArg.fixed scopeRet.fixed)
      <> scopeArg.floating
      <> scopeRet.floating

    removeQuant =
      mapQuantifier $ OSet.filter (_ `OSet.notElem` (unwrap newFixed))

  in
    TsTypeFunction (TsTypeArgsQuant $ coerce newFixed)
      [ TsFnArg (TsName "_") (removeQuant arg)
      ]
      (removeQuant ret)
  where
  mapAccum = over TsBridgeAccum (\x -> x { scope = fixScope x.scope })

-------------------------------------------------------------------------------
-- Class / DefaultRecord
-------------------------------------------------------------------------------

class DefaultRecord :: Type -> Row Type -> Constraint
class DefaultRecord mp r where
  defaultRecord :: mp -> Record r -> TsBridgeM TsType

instance (RowToList r rl, DefaultRecordRL mp rl) => DefaultRecord mp r where
  defaultRecord mp _ = TsTypeRecord <$> defaultRecordRL mp (Proxy :: _ rl)

-------------------------------------------------------------------------------
-- Class / DefaultRecordRL
-------------------------------------------------------------------------------

class DefaultRecordRL :: Type -> RowList Type -> Constraint
class DefaultRecordRL mp rl where
  defaultRecordRL :: mp -> Proxy rl -> TsBridgeM (Array TsRecordField)

instance DefaultRecordRL mp Nil where
  defaultRecordRL _ _ = pure []

instance
  ( ToTsBridgeBy mp (Proxy t)
  , DefaultRecordRL mp rl
  , IsSymbol s
  ) =>
  DefaultRecordRL mp (Cons s t rl) where
  defaultRecordRL mp _ = do
    let
      mkX = toTsBridgeBy mp (Proxy :: _ t)
      mkXs = defaultRecordRL mp (Proxy :: _ rl)
      str = reflectSymbol (Proxy :: _ s)
    x <- mkX
    xs <- mkXs
    let k = TsName $ str
    pure $
      A.cons (TsRecordField k { optional: false, readonly: true } x) xs

defaultOpaqueType :: forall a. String -> String -> Array String -> Array (TsBridgeM TsType) -> a -> TsBridgeM TsType
defaultOpaqueType pursModuleName pursTypeName targNames targs _ = brandedType
  (TsFilePath (pursModuleName <> "/index") "d.ts")
  (TsModuleAlias $ dotsToLodashes pursModuleName)
  (TsName pursTypeName)
  (OSet.fromFoldable $ TsName <$> targNames)
  targs
  Nothing

defaultBrandedType
  :: forall mp a t
   . Newtype a t
  => ToTsBridgeBy mp t
  => mp
  -> String
  -> String
  -> Array String
  -> Array (TsBridgeM TsType)
  -> a
  -> TsBridgeM TsType
defaultBrandedType mp pursModuleName pursTypeName targNames targs t = do
  x <- toTsBridgeBy mp $ unwrap t
  brandedType
    (TsFilePath (pursModuleName <> "/index") "d.ts")
    (TsModuleAlias $ dotsToLodashes pursModuleName)
    (TsName pursTypeName)
    (OSet.fromFoldable $ TsName <$> targNames)
    targs
    (Just x)

-------------------------------------------------------------------------------
-- Default Implementations
-------------------------------------------------------------------------------

fixScope :: Scope -> Scope
fixScope { fixed, floating } =
  { floating: mempty
  , fixed: fixed <> floating
  }

-------------------------------------------------------------------------------
-- Util
-------------------------------------------------------------------------------

brandedType :: TsFilePath -> TsModuleAlias -> TsName -> OSet TsName -> Array (TsBridgeM TsType) -> Maybe TsType -> TsBridgeM TsType
brandedType filePath moduleAlias name targs args' type_ = do
  args <- sequence args'

  let
    imports = Set.singleton $
      TsImport
        moduleAlias
        ( filePath
            # filePathToModulePath
            # (\(TsModulePath x) -> TsModulePath ("~/" <> x))
        )

    typeDefs =
      [ TsModuleFile
          filePath
          ( TsModule Set.empty
              [ mkBrandedTypeDecl name targs type_
              ]
          )
      ]

  tell
    $ TsBridgeAccum
    $ R.union mempty { typeDefs, imports }

  pure
    $ TsTypeConstructor (TsQualName (Just moduleAlias) name) (TsTypeArgs args)

dotsToLodashes :: String -> String
dotsToLodashes = Str.replaceAll (Pattern ".") (Replacement "_")

filePathToModulePath :: TsFilePath -> TsModulePath
filePathToModulePath (TsFilePath x _) = TsModulePath x

mkBrandedTypeDecl :: TsName -> OSet TsName -> Maybe TsType -> TsDeclaration
mkBrandedTypeDecl name args type_ = TsDeclTypeDef name Public (coerce args)
  $ maybeWithType
  $
    TsTypeRecord
      (opaqueField : (mapWithIndex mkArgFields $ OSet.toUnfoldable args))

  where
  maybeWithType = case type_ of
    Nothing -> identity
    Just t -> \x -> TsTypeIntersection x t

  opaqueField = TsRecordField
    (TsName $ "brand")
    { optional: false, readonly: true }
    TsTypeUniqueSymbol

  mkArgFields idx name' = TsRecordField
    (TsName ("arg" <> show idx))
    { optional: false, readonly: true }
    (TsTypeVar name')
