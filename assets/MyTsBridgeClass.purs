module MyTsBridgeClass where

{-GEN:imports
{}
-}

import Prelude

import Prim.RowList (class RowToList)
import TsBridge as TSB
import Type.Proxy (Proxy(..))

{-GEN:END-}
class ToTsBridge a where
  toTsBridge :: a -> TSB.TsBridgeM TSB.TsType

instance ToTsBridge a => ToTsBridge (Proxy a) where
  toTsBridge = TSB.defaultProxy Mp

instance ToTsBridge Number where
  toTsBridge = TSB.defaultNumber

instance ToTsBridge String where
  toTsBridge = TSB.defaultString

instance ToTsBridge Boolean where
  toTsBridge = TSB.defaultBoolean

instance ToTsBridge a => ToTsBridge (Array a) where
  toTsBridge = TSB.defaultArray Mp

instance (ToTsBridge a, ToTsBridge b) => ToTsBridge (a -> b) where
  toTsBridge = TSB.defaultFunction Mp

instance (TSB.GenRecord MappingToTsBridge rl, RowToList r rl) => ToTsBridge (Record r) where
  toTsBridge = TSB.defaultRecord Mp

data MappingToTsBridge = Mp

instance ToTsBridge a => TSB.Mapping MappingToTsBridge a (TSB.TsBridgeM TSB.TsType) where
  mapping _ = toTsBridge

{-GEN:instances
{ "include": ["SampleApp.**.*"], "exclude": [] }
-}

{-GEN:END-}