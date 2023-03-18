module TsBridge.Core
  ( class DefaultRecord
  , class DefaultRecordRL
  , class ToTsBridgeBy
  , toTsBridgeBy
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
  , mergeTsPrograms
  , tsModuleFile
  , tsOpaqueType
  , tsProgram
  , tsTypeAlias
  , tsUnsupported
  , tsValue
  ) where

import Prelude

import Control.Monad.Writer (listens, censor, listen, tell)
import Control.Promise (Promise)
import Data.Array (mapWithIndex, uncons, (:))
import Data.Array as A
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, over2, un, unwrap, wrap)
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
import TsBridge.DTS (TsBridge_DTS_Wrap(..), TsDeclVisibility(..), TsDeclaration(..), TsFilePath(..), TsFnArg(..), TsImport(..), TsModule(..), TsModuleAlias(..), TsModuleFile(..), TsModulePath(..), TsName(..), TsProgram(..), TsQualName(..), TsRecordField(..), TsType(..), TsTypeArgs(..), TsTypeArgsQuant(..), dtsFilePath, mapQuantifier)
import TsBridge.Monad (Scope, TsBridgeAccum(..), TsBridgeM, TsBridge_Monad_Wrap(..), defaultTsBridgeAccum, runTsBridgeM)
import TsBridge.TypeVars (Var)
import Type.Proxy (Proxy(..))

class ToTsBridgeBy tok a where
  toTsBridgeBy :: tok -> a -> TsBridgeM TsType

tsModuleFile :: String -> Array (TsBridgeM (Array TsDeclaration)) -> Array TsModuleFile
tsModuleFile n xs =
  let
    (xs' /\ TsBridgeAccum { typeDefs, imports }) = runTsBridgeM $ join <$> sequence xs
  in
    typeDefs <> [ TsModuleFile (dtsFilePath n) (TsModule imports xs') ]

mergeModules :: Array TsModuleFile -> TsProgram
mergeModules xs =
  xs
    <#> (\(TsModuleFile mp m) -> mp /\ m)
    # Map.fromFoldableWith mergeModule
    # TsProgram

mergeModule :: TsModule -> TsModule -> TsModule
mergeModule (TsModule is1 ds1) (TsModule is2 ds2) =
  TsModule
    (is1 `Set.union` is2)
    (Array.nub (ds1 <> ds2))

tsProgram :: Array (Array TsModuleFile) -> TsProgram
tsProgram xs = mergeModules $ join xs

tsTypeAlias :: forall mp a. ToTsBridgeBy mp a => mp -> String -> a -> TsBridgeM (Array TsDeclaration)
tsTypeAlias mp n x = ado
  x /\ scope <- listens (un TsBridgeAccum >>> _.scope) t
  in [ TsDeclTypeDef (TsName n) Public (coerce scope.floating) x ]
  where
  t = toTsBridgeBy mp x

tsOpaqueType :: forall mp a. ToTsBridgeBy mp a => mp -> String -> a -> TsBridgeM (Array TsDeclaration)
tsOpaqueType mp n x = do
  _ /\ modules <- listens (un TsBridgeAccum >>> _.typeDefs) $ toTsBridgeBy mp x
  case uncons modules of
    Just { head: (TsModuleFile _ (TsModule imports decls)), tail: [] } -> do
      tell $ TsBridgeAccum
        { typeDefs: mempty
        , imports
        , scope: mempty
        }
      pure decls
    _ -> pure []

tsValue :: forall mp a. ToTsBridgeBy mp a => mp -> String -> a -> TsBridgeM (Array TsDeclaration)
tsValue mp n x = do
  t <- toTsBridgeBy mp x
  pure [ TsDeclValueDef (TsName n) Public t ]

tsUnsupported :: String -> String -> TsBridgeM (Array TsDeclaration)
tsUnsupported x reason = pure
  [ TsDeclComments [ "`" <> x <> "` is unsupported: " <> reason ]
  ]

mergeTsPrograms :: TsProgram -> TsProgram -> TsProgram
mergeTsPrograms p1 p2 = p1 --TODO

-------------------------------------------------------------------------------
-- Util
-------------------------------------------------------------------------------

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

filePathToModulePath :: TsFilePath -> TsModulePath
filePathToModulePath (TsFilePath x _) = TsModulePath x

-------------------------------------------------------------------------------
-- Default Implementations
-------------------------------------------------------------------------------

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

defaultProxy :: forall f a. ToTsBridgeBy f a  => f -> Proxy a -> TsBridgeM TsType
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

fixScope :: Scope -> Scope
fixScope { fixed, floating } =
  { floating: mempty
  , fixed: fixed <> floating
  }

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

-------------------------------------------------------------------------------
-- Util
-------------------------------------------------------------------------------

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