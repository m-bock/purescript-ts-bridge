module TsBridge.Class
  ( class GenRecord
  , class GenRecordRL
  , defaultArray
  , defaultBoolean
  , defaultFunction
  , defaultNumber
  , defaultString
  , genRecord
  , genRecordRL
  , tsOpaqueType
  , tsTypeVar
  )
  where

import Prelude

import Control.Monad.Writer (censor, listen, tell)
import Data.Array as A
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Newtype (over, over2, unwrap, wrap)
import Data.Set.Ordered as OSet
import Data.String (Pattern(..), Replacement(..))
import Data.String as Str
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Undefined (undefined)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Safe.Coerce (coerce)
import TsBridge.ABC (A, B, C)
import TsBridge.DTS (TsFilePath(..), TsFnArg(..), TsModuleAlias(..), TsName(..), TsRecordField(..), TsType(..), TsTypeArgsQuant(..), mapQuantifier)
import TsBridge.DTS as TsBridge.DTS
import TsBridge.Monad (Scope, TsBridgeAccum(..), TsBridgeM, TsBridge_Monad_Wrap(..), defaultTsBridgeAccum, opaqueType)
import Type.Proxy (Proxy(..))

-------------------------------------------------------------------------------
-- Class / ToTsBridge
-------------------------------------------------------------------------------

class ToTsBridge a where
  toTsBridge :: a -> TsBridgeM TsType

-------------------------------------------------------------------------------
-- Class / ToTsBridge / Proxy
-------------------------------------------------------------------------------

instance ToTsBridge a => ToTsBridge (Proxy a) where
  toTsBridge _ = toTsBridge (undefined :: a)

-------------------------------------------------------------------------------
-- Class / ToTsBridge / Primitives
-------------------------------------------------------------------------------

instance ToTsBridge Number where
  toTsBridge _ = defaultNumber

instance ToTsBridge String where
  toTsBridge _ = defaultString

instance ToTsBridge Boolean where
  toTsBridge _ = defaultBoolean

instance ToTsBridge a => ToTsBridge (Array a) where
  toTsBridge _ = defaultArray (toTsBridge (Proxy :: _ a))

instance (GenRecord r) => ToTsBridge (Record r) where
  toTsBridge _ = defaultRecord (Proxy :: _ r)

instance (ToTsBridge a, ToTsBridge b) => ToTsBridge (a -> b) where
  toTsBridge _ = defaultFunction
    (toTsBridge (Proxy :: _ a))
    (toTsBridge (Proxy :: _ b))

instance ToTsBridge a => ToTsBridge (Maybe a) where
  toTsBridge _ = tsOpaqueType "Data.Maybe" "Maybe" [ "A" ]
    [ toTsBridge (Proxy :: _ a) ]

instance (ToTsBridge a, ToTsBridge b) => ToTsBridge (Either a b) where
  toTsBridge _ = tsOpaqueType "Data.Either" "Either" [ "A", "B" ]
    [ toTsBridge (Proxy :: _ a)
    , toTsBridge (Proxy :: _ b)
    ]

instance ToTsBridge A where
  toTsBridge _ = tsTypeVar "A"

instance ToTsBridge B where
  toTsBridge _ = tsTypeVar "B"

instance ToTsBridge C where
  toTsBridge _ = tsTypeVar "C"

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

defaultNumber :: TsBridgeM TsType
defaultNumber = pure TsTypeNumber

defaultString :: TsBridgeM TsType
defaultString = pure TsTypeString

defaultBoolean :: TsBridgeM TsType
defaultBoolean = pure TsTypeBoolean

defaultArray :: TsBridgeM TsType -> TsBridgeM TsType
defaultArray a = TsTypeArray <$> a

defaultFunction :: TsBridgeM TsType -> TsBridgeM TsType -> TsBridgeM TsType
defaultFunction a b = censor mapAccum ado
  arg /\ TsBridgeAccum { scope: scopeArg } <- listen $ a
  ret /\ TsBridgeAccum { scope: scopeRet } <- listen $ b
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

defaultRecord :: forall r. (GenRecord r) => Proxy r -> TsBridgeM TsType
defaultRecord p = TsTypeRecord <$> genRecord p

fixScope :: Scope -> Scope
fixScope { fixed, floating } =
  { floating: mempty
  , fixed: fixed <> floating
  }

-------------------------------------------------------------------------------
-- Class / GenRecord
-------------------------------------------------------------------------------

class GenRecord :: Row Type -> Constraint
class GenRecord r where
  genRecord :: Proxy r -> TsBridgeM (Array TsRecordField)

instance (RowToList r rl, GenRecordRL rl) => GenRecord r
  where
  genRecord _ = genRecordRL (Proxy :: _ rl)

class GenRecordRL :: RowList Type -> Constraint
class GenRecordRL rl where
  genRecordRL :: Proxy rl -> TsBridgeM (Array TsRecordField)

instance GenRecordRL Nil where
  genRecordRL _ = pure []

instance (GenRecordRL rl, ToTsBridge t, IsSymbol s) => GenRecordRL (Cons s t rl) where
  genRecordRL _ = ado
    x <- toTsBridge (Proxy :: _ t)
    xs <- genRecordRL (Proxy :: _ rl)
    let k = TsName $ reflectSymbol (Proxy :: _ s)
    in
      A.cons (TsRecordField k { optional: false, readonly: true } x) xs

-------------------------------------------------------------------------------
-- Util
-------------------------------------------------------------------------------

tsOpaqueType :: String -> String -> Array String -> Array (TsBridgeM TsType) -> TsBridgeM TsType
tsOpaqueType pursModuleName pursTypeName targs = opaqueType
  (TsFilePath (pursModuleName <> "/index") "d.ts")
  (TsModuleAlias $ dotsToLodashes pursModuleName)
  (TsName pursTypeName)
  (OSet.fromFoldable $ TsName <$> targs)

dotsToLodashes :: String -> String
dotsToLodashes = Str.replaceAll (Pattern ".") (Replacement "_")