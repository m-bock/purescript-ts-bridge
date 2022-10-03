module MyTsBridgeClass where

{-GEN:imports
{ "autoPrefix": "auto" }
-}

import Data.Either (Either)
import Heterogeneous.Mapping (class Mapping)
import TsBridge (TsBridgeM, TsType, defaultProxy, tsOpaqueType, tsOpaqueType2)
import TsBridge.Core (tsNewtype)
import Type.Proxy (Proxy)


{-GEN:END-}

class ToTsBridge a where
  toTsBridge :: a -> TsBridgeM TsType

data MappingToTsBridge = Mp

instance ToTsBridge a => Mapping MappingToTsBridge a (TsBridgeM TsType) where
  mapping _ = toTsBridge

instance ToTsBridge a => ToTsBridge (Proxy a) where
  toTsBridge = defaultProxy Mp

instance (ToTsBridge a, ToTsBridge b) => ToTsBridge (Either a b) where
  toTsBridge = tsOpaqueType2 Mp "Data.Either" "Either" "A" "B"

{-GEN:instances
{ "include": [ "**" ]
, "exclude": []
}
-}

{-GEN:END-}

-- instance ToTsBridge Number where
--   toTsBridge _ = defaultNumber
