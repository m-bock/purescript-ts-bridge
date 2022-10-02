module TsBridge.Core
  ( class GenRecord
  , defaultArray
  , defaultBoolean
  , defaultEffect
  , defaultFunction
  , defaultNumber
  , defaultProxy
  , defaultRecord
  , defaultString
  , defaultUnit
  , genRecord
  , tsModuleFile
  , tsNewtype
  , tsOpaqueType
  , tsOpaqueType1
  , tsOpaqueType2
  , tsOpaqueType3
  , tsOpaqueType4
  , tsOpaqueType5
  , tsOpaqueType6
  , tsProgram
  , tsTypeAlias
  , tsTypeVar
  , tsValue
  ) where

import Prelude

import Control.Monad.Writer (listens, censor, listen, tell)
import Data.Array (mapWithIndex, (:))
import Data.Array as A
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (over, over2, unwrap, wrap, un)
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
import Heterogeneous.Mapping (class Mapping, mapping)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Record as R
import Safe.Coerce (coerce)
import TsBridge.DTS (TsBridge_DTS_Wrap(..), TsDeclVisibility(..), TsDeclaration(..), TsFilePath(..), TsFnArg(..), TsImport(..), TsModule(..), TsModuleAlias(..), TsModuleFile(..), TsModulePath(..), TsName(..), TsProgram(..), TsQualName(..), TsRecordField(..), TsType(..), TsTypeArgs(..), TsTypeArgsQuant(..), dtsFilePath, mapQuantifier)
import TsBridge.Monad (TsBridgeAccum(..), TsBridgeM, runTsBridgeM, Scope, TsBridge_Monad_Wrap(..), defaultTsBridgeAccum)
import TsBridge.Print (printTsName)
import Type.Proxy (Proxy(..))

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

tsTypeAlias :: forall mp a. Mapping mp a (TsBridgeM TsType) => mp -> String -> a -> TsBridgeM (Array TsDeclaration)
tsTypeAlias mp n x = ado
  x /\ scope <- listens (un TsBridgeAccum >>> _.scope) t
  in [ TsDeclTypeDef (TsName n) Public (coerce scope.floating) x ]
  where
  t = mapping mp x

tsValue :: forall mp a. Mapping mp a (TsBridgeM TsType) => mp -> String -> a -> TsBridgeM (Array TsDeclaration)
tsValue mp n x = do
  t <- mapping mp x
  pure [ TsDeclValueDef (TsName n) Public t ]

-------------------------------------------------------------------------------
-- Util
-------------------------------------------------------------------------------

opaqueType :: TsFilePath -> TsModuleAlias -> TsName -> OSet TsName -> Array (TsBridgeM TsType) -> TsBridgeM TsType
opaqueType filePath moduleAlias name targs args' = do
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
              [ mkOpaqueTypeDecl name targs
              ]
          )
      ]

  tell
    $ TsBridgeAccum
    $ R.union mempty { typeDefs, imports }

  pure
    $ TsTypeConstructor (TsQualName (Just moduleAlias) name) (TsTypeArgs args)

mkOpaqueTypeDecl :: TsName -> OSet TsName -> TsDeclaration
mkOpaqueTypeDecl name args = TsDeclTypeDef name Public (coerce args) $
  TsTypeRecord
    (opaqueField : (mapWithIndex mkArgFields $ OSet.toUnfoldable args))

  where
  opaqueField = TsRecordField
    (TsName $ "opaque_" <> printTsName name)
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

tsTypeVar :: String -> TsBridgeM TsType
tsTypeVar x = do
  let
    tsName = TsName x

    scope =
      { floating: wrap $ OSet.singleton tsName
      , fixed: mempty
      }

  tell
    $ over TsBridgeAccum _ { scope = scope } defaultTsBridgeAccum
  pure $ TsTypeVar tsName

defaultProxy :: forall f a. Mapping f a (TsBridgeM TsType) => f -> Proxy a -> TsBridgeM TsType
defaultProxy mp _ = mapping mp (undefined :: a)

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
   . Mapping f (Proxy a) (TsBridgeM TsType)
  => f
  -> Effect a
  -> TsBridgeM TsType
defaultEffect f _ = do
  x <- (mapping f (Proxy :: _ a))
  pure $ TsTypeFunction
    (TsTypeArgsQuant $ coerce $ Oset.singleton $ TsName "A")
    []
    x

defaultArray
  :: forall a f
   . Mapping f (Proxy a) (TsBridgeM TsType)
  => f
  -> Array a
  -> TsBridgeM TsType
defaultArray f _ = TsTypeArray <$> mapping f (Proxy :: _ a)

defaultPromise
  :: forall a f
   . Mapping f (Proxy a) (TsBridgeM TsType)
  => f
  -> Array a
  -> TsBridgeM TsType
defaultPromise f _ = TsTypeArray <$> mapping f (Proxy :: _ a)

defaultFunction
  :: forall f a b
   . Mapping f (Proxy a) (TsBridgeM TsType)
  => Mapping f (Proxy b) (TsBridgeM TsType)
  => f
  -> (a -> b)
  -> TsBridgeM TsType
defaultFunction f _ = censor mapAccum ado
  arg /\ TsBridgeAccum { scope: scopeArg } <- listen $ mapping f (Proxy :: _ a)
  ret /\ TsBridgeAccum { scope: scopeRet } <- listen $ mapping f (Proxy :: _ b)
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

defaultRecord
  :: forall mp r rl
   . GenRecord mp rl
  => RowToList r rl
  => mp
  -> { | r }
  -> TsBridgeM TsType
defaultRecord mp _ = TsTypeRecord <$> genRecord mp (Proxy :: _ rl)

fixScope :: Scope -> Scope
fixScope { fixed, floating } =
  { floating: mempty
  , fixed: fixed <> floating
  }

-------------------------------------------------------------------------------
-- Class / GenRecord
-------------------------------------------------------------------------------

class GenRecord :: Type -> RowList Type -> Constraint
class GenRecord mp rl where
  genRecord :: mp -> Proxy rl -> TsBridgeM (Array TsRecordField)

instance GenRecord mp Nil where
  genRecord _ _ = pure []

instance
  ( Mapping mp (Proxy t) (TsBridgeM TsType)
  , GenRecord mp rl
  , IsSymbol s
  ) =>
  GenRecord mp (Cons s t rl) where
  genRecord mp _ = do
    let
      mkX = mapping mp (Proxy :: _ t)
      mkXs = genRecord mp (Proxy :: _ rl)
      str = reflectSymbol (Proxy :: _ s)
    x <- mkX
    xs <- mkXs
    let k = TsName $ str
    pure $
      A.cons (TsRecordField k { optional: false, readonly: true } x) xs

-------------------------------------------------------------------------------
-- Util
-------------------------------------------------------------------------------

tsOpaqueTypeImpl :: String -> String -> Array String -> Array (TsBridgeM TsType) -> TsBridgeM TsType
tsOpaqueTypeImpl pursModuleName pursTypeName targs = opaqueType
  (TsFilePath (pursModuleName <> "/index") "d.ts")
  (TsModuleAlias $ dotsToLodashes pursModuleName)
  (TsName pursTypeName)
  (OSet.fromFoldable $ TsName <$> targs)

tsNewtype = undefined

tsOpaqueType
  :: forall a
   . String
  -> String
  -> a
  -> TsBridgeM TsType
tsOpaqueType pursModuleName pursTypeName _ =
  tsOpaqueTypeImpl pursModuleName pursTypeName [] []

tsOpaqueType1
  :: forall a mp f
   . Mapping mp (Proxy a) (TsBridgeM TsType)
  => mp
  -> String
  -> String
  -> String
  -> f a
  -> TsBridgeM TsType
tsOpaqueType1 mp pursModuleName pursTypeName a _ =
  tsOpaqueTypeImpl pursModuleName pursTypeName [ a ] [ mapping mp (Proxy :: _ a) ]

tsOpaqueType2
  :: forall a1 a2 mp f
   . Mapping mp (Proxy a1) (TsBridgeM TsType)
  => Mapping mp (Proxy a2) (TsBridgeM TsType)
  => mp
  -> String
  -> String
  -> String
  -> String
  -> f a1 a2
  -> TsBridgeM TsType
tsOpaqueType2 mp pursModuleName pursTypeName a1 a2 _ =
  tsOpaqueTypeImpl pursModuleName pursTypeName [ a1, a2 ]
    [ mapping mp (Proxy :: _ a1)
    , mapping mp (Proxy :: _ a2)
    ]

tsOpaqueType3
  :: forall a1 a2 a3 mp f
   . Mapping mp (Proxy a1) (TsBridgeM TsType)
  => Mapping mp (Proxy a2) (TsBridgeM TsType)
  => Mapping mp (Proxy a3) (TsBridgeM TsType)
  => mp
  -> String
  -> String
  -> String
  -> String
  -> String
  -> f a1 a2 a3
  -> TsBridgeM TsType
tsOpaqueType3 mp pursModuleName pursTypeName a1 a2 a3 _ =
  tsOpaqueTypeImpl pursModuleName pursTypeName [ a1, a2, a3 ]
    [ mapping mp (Proxy :: _ a1)
    , mapping mp (Proxy :: _ a2)
    , mapping mp (Proxy :: _ a3)
    ]

tsOpaqueType4
  :: forall a1 a2 a3 a4 mp f
   . Mapping mp (Proxy a1) (TsBridgeM TsType)
  => Mapping mp (Proxy a2) (TsBridgeM TsType)
  => Mapping mp (Proxy a3) (TsBridgeM TsType)
  => Mapping mp (Proxy a4) (TsBridgeM TsType)
  => mp
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> f a1 a2 a3 a4
  -> TsBridgeM TsType
tsOpaqueType4 mp pursModuleName pursTypeName a1 a2 a3 a4 _ =
  tsOpaqueTypeImpl pursModuleName pursTypeName [ a1, a2, a3, a4 ]
    [ mapping mp (Proxy :: _ a1)
    , mapping mp (Proxy :: _ a2)
    , mapping mp (Proxy :: _ a3)
    , mapping mp (Proxy :: _ a4)
    ]

tsOpaqueType5
  :: forall a1 a2 a3 a4 a5 mp f
   . Mapping mp (Proxy a1) (TsBridgeM TsType)
  => Mapping mp (Proxy a2) (TsBridgeM TsType)
  => Mapping mp (Proxy a3) (TsBridgeM TsType)
  => Mapping mp (Proxy a4) (TsBridgeM TsType)
  => Mapping mp (Proxy a5) (TsBridgeM TsType)
  => mp
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> f a1 a2 a3 a4 a5
  -> TsBridgeM TsType
tsOpaqueType5 mp pursModuleName pursTypeName a1 a2 a3 a4 a5 _ =
  tsOpaqueTypeImpl pursModuleName pursTypeName [ a1, a2, a3, a4, a5 ]
    [ mapping mp (Proxy :: _ a1)
    , mapping mp (Proxy :: _ a2)
    , mapping mp (Proxy :: _ a3)
    , mapping mp (Proxy :: _ a4)
    , mapping mp (Proxy :: _ a5)
    ]

tsOpaqueType6
  :: forall a1 a2 a3 a4 a5 a6 mp f
   . Mapping mp (Proxy a1) (TsBridgeM TsType)
  => Mapping mp (Proxy a2) (TsBridgeM TsType)
  => Mapping mp (Proxy a3) (TsBridgeM TsType)
  => Mapping mp (Proxy a4) (TsBridgeM TsType)
  => Mapping mp (Proxy a5) (TsBridgeM TsType)
  => Mapping mp (Proxy a6) (TsBridgeM TsType)
  => mp
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> f a1 a2 a3 a4 a5
  -> TsBridgeM TsType
tsOpaqueType6 mp pursModuleName pursTypeName a1 a2 a3 a4 a5 a6 _ =
  tsOpaqueTypeImpl pursModuleName pursTypeName [ a1, a2, a3, a4, a5, a6 ]
    [ mapping mp (Proxy :: _ a1)
    , mapping mp (Proxy :: _ a2)
    , mapping mp (Proxy :: _ a3)
    , mapping mp (Proxy :: _ a4)
    , mapping mp (Proxy :: _ a5)
    , mapping mp (Proxy :: _ a6)
    ]

dotsToLodashes :: String -> String
dotsToLodashes = Str.replaceAll (Pattern ".") (Replacement "_")