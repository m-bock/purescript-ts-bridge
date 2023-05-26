module TsBridge.Class where

import Prelude

import Control.Promise (Promise)
import DTS as DTS
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple)
import Data.Variant (Variant)
import Effect (Effect)
import TsBridge as TSB
import Type.Proxy (Proxy)

class TsBridge (a :: Type) where
  tsBridge :: Proxy a -> TSB.TsBridgeM DTS.TsType

data Tok = Tok

instance TsBridge a => TSB.TsBridgeBy Tok a where
  tsBridgeBy _ = tsBridge

instance (TsBridge a, TsBridge b) => TsBridge (Either a b) where
  tsBridge = TSB.tsBridgeEither Tok

instance (TsBridge a, TsBridge b) => TsBridge (Tuple a b) where
  tsBridge = TSB.tsBridgeTuple Tok

instance TsBridge Number where
  tsBridge = TSB.tsBridgeNumber

instance (TSB.TsBridgeRecord Tok r) => TsBridge (Record r) where
  tsBridge = TSB.tsBridgeRecord Tok

instance (TSB.TsBridgeVariant Tok r) => TsBridge (Variant r) where
  tsBridge = TSB.tsBridgeVariant Tok

instance TsBridge String where
  tsBridge = TSB.tsBridgeString

instance TsBridge Boolean where
  tsBridge = TSB.tsBridgeBoolean

instance TsBridge Int where
  tsBridge = TSB.tsBridgeInt

instance TsBridge Char where
  tsBridge = TSB.tsBridgeChar

instance TsBridge Unit where
  tsBridge = TSB.tsBridgeUnit

instance TsBridge a => TsBridge (Array a) where
  tsBridge = TSB.tsBridgeArray Tok

instance TsBridge a => TsBridge (Effect a) where
  tsBridge = TSB.tsBridgeEffect Tok

instance TsBridge a => TsBridge (Nullable a) where
  tsBridge = TSB.tsBridgeNullable Tok

instance (TsBridge a, TsBridge b) => TsBridge (a -> b) where
  tsBridge = TSB.tsBridgeFunction Tok

instance TsBridge a => TsBridge (Maybe a) where
  tsBridge = TSB.tsBridgeMaybe Tok

instance TsBridge a => TsBridge (Promise a) where
  tsBridge = TSB.tsBridgePromise Tok

instance IsSymbol sym => TsBridge (TSB.TypeVar sym) where
  tsBridge = TSB.tsBridgeTypeVar