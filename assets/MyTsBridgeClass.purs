module MyTsBridgeClass where

{-GEN:imports
{}
-}

import Data.Either (Either)
import TsBridge (class Mapping, TsBridgeM, TsType, defaultOpaqueType, defaultProxy, tsOpaqueType)
import Type.Proxy (Proxy(..))

{-GEN:END-}

class ToTsBridge a where
  toTsBridge :: a -> TsBridgeM TsType

data MappingToTsBridge = Mp

instance ToTsBridge a => Mapping MappingToTsBridge a (TsBridgeM TsType) where
  mapping _ = toTsBridge

instance ToTsBridge a => ToTsBridge (Proxy a) where
  toTsBridge = defaultProxy Mp

instance (ToTsBridge a, ToTsBridge b) => ToTsBridge (Either a b) where
  toTsBridge = defaultOpaqueType "Data.Either" "Either" [ "A", "B" ]
    [ toTsBridge (Proxy :: _ a), toTsBridge (Proxy :: _ b) ]

{-GEN:instances
{ "include": [ "**" ]
, "exclude": []
}
-}

{-GEN:END-}

-- instance ToTsBridge Number where
--   toTsBridge _ = defaultNumber
