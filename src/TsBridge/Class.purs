module TsBridge.Class where

import Prelude

import Control.Promise (Promise)
import DTS as DTS
import Data.Either (Either)
import Data.Function.Uncurried (Fn2, Fn3)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple)
import Data.Variant (Variant)
import Data.Variant.Encodings.Flat (VariantEncodedFlat)
import Data.Variant.Encodings.Nested (VariantEncodedNested)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, EffectFn3)
import Foreign.Object (Object)
import Literals (StringLit)
import Literals.Null (Null)
import Literals.Undefined (Undefined)
import TsBridge.Types.Intersection (Intersection, tsBridgeIntersection)
import TsBridge.Core (class TsBridgeBy)
import TsBridge.DefaultImpls as TSB
import TsBridge.Monad (TsBridgeM)
import TsBridge.Types.TsRecord (TsRecord, class TsBridgeTsRecord, tsBridgeTsRecord)
import Type.Proxy (Proxy)
import Untagged.Union (OneOf)

class TsBridge (a :: Type) where
  tsBridge :: Proxy a -> TsBridgeM DTS.TsType

data Tok = Tok

instance TsBridge a => TsBridgeBy Tok a where
  tsBridgeBy _ = tsBridge

instance (TsBridge a, TsBridge b) => TsBridge (Either a b) where
  tsBridge = TSB.tsBridgeEither Tok

instance (TsBridge a, TsBridge b) => TsBridge (Tuple a b) where
  tsBridge = TSB.tsBridgeTuple Tok

instance TsBridge Number where
  tsBridge = TSB.tsBridgeNumber

instance (TSB.TsBridgeRecord Tok r) => TsBridge (Record r) where
  tsBridge = TSB.tsBridgeRecord Tok

instance (TsBridgeTsRecord Tok r) => TsBridge (TsRecord r) where
  tsBridge = tsBridgeTsRecord Tok

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

instance (TsBridge a, TsBridge b, TsBridge c) => TsBridge (Fn2 a b c) where
  tsBridge = TSB.tsBridgeFn2 Tok

instance (TsBridge a, TsBridge b, TsBridge c, TsBridge d) => TsBridge (Fn3 a b c d) where
  tsBridge = TSB.tsBridgeFn3 Tok

instance (TsBridge a, TsBridge b, TsBridge c) => TsBridge (EffectFn2 a b c) where
  tsBridge = TSB.tsBridgeEffectFn2 Tok

instance (TsBridge a, TsBridge b, TsBridge c, TsBridge d) => TsBridge (EffectFn3 a b c d) where
  tsBridge = TSB.tsBridgeEffectFn3 Tok

instance TsBridge a => TsBridge (Maybe a) where
  tsBridge = TSB.tsBridgeMaybe Tok

instance TsBridge a => TsBridge (Promise a) where
  tsBridge = TSB.tsBridgePromise Tok

instance IsSymbol sym => TsBridge (TSB.TypeVar sym) where
  tsBridge = TSB.tsBridgeTypeVar

instance (TsBridge a, TsBridge b) => TsBridge (OneOf a b) where
  tsBridge = TSB.tsBridgeOneOf Tok

instance (TsBridge a, TsBridge b) => TsBridge (Intersection a b) where
  tsBridge = tsBridgeIntersection Tok

instance (TSB.TsBridgeVariantEncodedFlat Tok symTag r) => TsBridge (VariantEncodedFlat symTag r) where
  tsBridge = TSB.tsBridgeVariantEncodedFlat Tok

instance (TSB.TsBridgeVariantEncodedNested Tok symTag symVal r) => TsBridge (VariantEncodedNested symTag symVal r) where
  tsBridge = TSB.tsBridgeVariantEncodedNested Tok

instance TsBridge Undefined where
  tsBridge = TSB.tsBridgeUndefined

instance TsBridge Null where
  tsBridge = TSB.tsBridgeNull

instance IsSymbol sym => TsBridge (StringLit sym) where
  tsBridge = TSB.tsBridgeStringLit
