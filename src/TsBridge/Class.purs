module TsBridge.Class
  ( class TsBridge
  , toTsType
  ) where

import Prelude

import Data.Typelevel.Undefined (undefined)
import TsBridge.DTS (TsType(..))
import Type.Proxy (Proxy(..))

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

instance TsBridge Boolean where
  toTsType _ = TsTypeBoolean

instance TsBridge a => TsBridge (Array a) where
  toTsType _ = TsTypeArray $ toTsType (Proxy :: _ a)
