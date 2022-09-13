module TsBridge.Class
  ( class GenRecord
  , class ToTsBridge
  , genRecord
  , toTsBridge
  ) where

import Prelude

import Data.Array as A
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Set as Set
import Data.String (Pattern(..), Replacement(..))
import Data.String as Str
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Typelevel.Undefined (undefined)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import TsBridge.DTS (TsFilePath(..), TsFnArg(..), TsModuleAlias(..), TsName(..), TsRecordField(..), TsType(..), TsTypeArgsQuant(..))
import TsBridge.Monad (TsBridgeM, opaqueType)
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
  toTsBridge _ = pure TsTypeNumber

instance ToTsBridge String where
  toTsBridge _ = pure TsTypeString

instance ToTsBridge Boolean where
  toTsBridge _ = pure TsTypeBoolean

instance ToTsBridge a => ToTsBridge (Array a) where
  toTsBridge _ = TsTypeArray <$> toTsBridge (Proxy :: _ a)

instance (RowToList r rl, GenRecord rl) => ToTsBridge (Record r) where
  toTsBridge _ = TsTypeRecord <$> genRecord (Proxy :: _ rl)

instance (ToTsBridge a, ToTsBridge b) => ToTsBridge (a -> b) where
  toTsBridge _ = ado
    arg <- toTsBridge (Proxy :: _ a)
    ret <- toTsBridge (Proxy :: _ b)
    in
      TsTypeFunction (TsTypeArgsQuant Set.empty)
        [ TsFnArg (TsName "_") arg ]
        ret

-------------------------------------------------------------------------------
-- Class / ToTsBridge / Standard Types
-------------------------------------------------------------------------------

instance ToTsBridge a => ToTsBridge (Maybe a) where
  toTsBridge _ = tsOpaqueType "Data.Maybe" "Maybe" [ "A" ]
    [ toTsBridge (Proxy :: _ a) ]

instance (ToTsBridge a, ToTsBridge b) => ToTsBridge (Either a b) where
  toTsBridge _ = tsOpaqueType "Data.Either" "Either" [ "A", "B" ]
    [ toTsBridge (Proxy :: _ a)
    , toTsBridge (Proxy :: _ b)
    ]

-------------------------------------------------------------------------------
-- Class / GenRecord
-------------------------------------------------------------------------------

class GenRecord :: RowList Type -> Constraint
class GenRecord rl where
  genRecord :: Proxy rl -> TsBridgeM (Array TsRecordField)

instance GenRecord Nil where
  genRecord _ = pure []

instance (GenRecord rl, ToTsBridge t, IsSymbol s) => GenRecord (Cons s t rl) where
  genRecord _ = ado
    x <- toTsBridge (Proxy :: _ t)
    xs <- genRecord (Proxy :: _ rl)
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
  (TsName <$> targs)

dotsToLodashes :: String -> String
dotsToLodashes = Str.replaceAll (Pattern ".") (Replacement "_")