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
import Data.Variant.Encodings.Flat (VariantEncodedFlat)
import Data.Variant.Encodings.Nested (VariantEncodedNested)
import Effect (Effect)
import Foreign.Object (Object)
import Literals (StringLit)
import Literals.Undefined as Lit
import TsBridge (Intersection, TsRecord)
import TsBridge as TSB
import Type.Proxy (Proxy)
import Untagged.Union (OneOf)

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

instance (TSB.TsBridgeTsRecord Tok r) => TsBridge (TsRecord r) where
  tsBridge = TSB.tsBridgeTsRecord Tok

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

instance TsBridge a => TsBridge (Object a) where
  tsBridge = TSB.tsBridgeObject Tok

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

instance (TsBridge a, TsBridge b) => TsBridge (OneOf a b) where
  tsBridge = TSB.tsBridgeOneOf Tok

instance (TsBridge a, TsBridge b) => TsBridge (Intersection a b) where
  tsBridge = TSB.tsBridgeIntersection Tok

instance (TSB.TsBridgeVariantEncodedFlat Tok symTag r) => TsBridge (VariantEncodedFlat symTag r) where
  tsBridge = TSB.tsBridgeVariantEncodedFlat Tok

instance (TSB.TsBridgeVariantEncodedNested Tok symTag symVal r) => TsBridge (VariantEncodedNested symTag symVal r) where
  tsBridge = TSB.tsBridgeVariantEncodedNested Tok

instance TsBridge Lit.Undefined where
  tsBridge = TSB.tsBridgeLitUndefined

instance IsSymbol sym => TsBridge (StringLit sym) where
  tsBridge = TSB.tsBridgeStringLit
