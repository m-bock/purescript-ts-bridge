module TsBridge.Class
  ( class GenRecord
  , class TsBridge
  , toTsType
  , genRecord
  ) where

import Prelude

import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Undefined (undefined)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import TsBridge.DTS (TsName(..), TsQualName(..), TsType(..))
import Type.Proxy (Proxy(..))

-------------------------------------------------------------------------------
-- TsBridge / Class
-------------------------------------------------------------------------------

class TsBridge a where
  toTsType :: a -> TsType

-------------------------------------------------------------------------------
-- TsBridge / Proxy
-------------------------------------------------------------------------------

instance TsBridge a => TsBridge (Proxy a) where
  toTsType _ = toTsType (undefined :: a)

-------------------------------------------------------------------------------
-- TsBridge / Primitives
-------------------------------------------------------------------------------

instance TsBridge Number where
  toTsType _ = TsTypeNumber

instance TsBridge String where
  toTsType _ = TsTypeString

instance TsBridge Boolean where
  toTsType _ = TsTypeBoolean

instance TsBridge a => TsBridge (Array a) where
  toTsType _ = TsTypeArray $ toTsType (Proxy :: _ a)

instance (RowToList r rl, GenRecord rl) => TsBridge (Record r) where
  toTsType _ = TsTypeRecord $ genRecord (Proxy :: _ rl)

instance (TsBridge a, TsBridge b) => TsBridge (a -> b) where
  toTsType _ =
    TsTypeFunction Set.empty
      [ TsName "_" /\ toTsType (Proxy :: _ a) ]
      (toTsType (Proxy :: _ b))

-------------------------------------------------------------------------------
-- TsBridge / Standard Types
-------------------------------------------------------------------------------

instance TsBridge a => TsBridge (Maybe a) where
  toTsType _ = TsTypeConstructor
    (TsQualName (Just "Data_Maybe") "Maybe")
    [ toTsType (Proxy :: _ a) ]

-------------------------------------------------------------------------------
-- GenRecord
-------------------------------------------------------------------------------

class GenRecord :: RowList Type -> Constraint
class GenRecord rl where
  genRecord :: Proxy rl -> Array (TsName /\ TsType)

instance GenRecord Nil where
  genRecord _ = []

instance (GenRecord rl, TsBridge t, IsSymbol s) => GenRecord (Cons s t rl) where
  genRecord _ =
    genRecord (Proxy :: _ rl)
      # A.cons (TsName (reflectSymbol (Proxy :: _ s)) /\ toTsType (Proxy :: _ t))
