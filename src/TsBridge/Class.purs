module TsBridge.Class
  ( class TsBridge
  , toTsType
  ) where

import Data.Typelevel.Undefined (undefined)
import TsBridge.DTS (TsType(..))
import Type.Proxy (Proxy)

class TsBridge a where
  toTsType :: a -> TsType

-------------------------------------------------------------------------------
-- Proxy
-------------------------------------------------------------------------------

instance TsBridge a => TsBridge (Proxy a) where
  toTsType _ = toTsType (undefined :: a)

-------------------------------------------------------------------------------
-- Primitives
-------------------------------------------------------------------------------

instance TsBridge Number where
  toTsType _ = TsTypeNumber

instance TsBridge String where
  toTsType _ = TsTypeString

