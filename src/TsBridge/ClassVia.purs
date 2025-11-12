module TsBridge.ClassVia where

import Control.Promise (Promise)
import DTS as DTS
import Data.Either (Either)
import Data.Function.Uncurried (Fn2, Fn3, Fn4)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Reflectable (class Reflectable)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple)
import Data.Variant (Variant)
import Data.Variant.Encodings.Flat (VariantEncodedFlat)
import Data.Variant.Encodings.Nested (VariantEncodedNested)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4)
import Foreign.Object (Object)
import Literals (StringLit, BooleanLit)
import Literals.Null (Null)
import Literals.Undefined (Undefined)
import TsBridge.Core (class TsBridgeBy, tsBridgeBy)
import TsBridge.DefaultImpls as TSB
import TsBridge.Monad (TsBridgeM)
import TsBridge.Types.Intersection (Intersection, tsBridgeIntersection)
import TsBridge.Types.Lit (Lit)
import TsBridge.Types.NTuple (NTuple)
import Type.Prelude (Proxy(..))
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)
import Untagged.Union (OneOf)

class TsBridgeVia (tok :: Type) (a :: Type) where
  tsBridgeVia :: tok -> Proxy a -> TsBridgeM DTS.TsType

instance TsBridgeBy tok a => TsBridgeVia tok (TSB.Named sym a) where
  tsBridgeVia tok _ = tsBridgeBy tok (Proxy :: _ a)

instance (TsBridgeBy tok a, TsBridgeBy tok b) => TsBridgeVia tok (Either a b) where
  tsBridgeVia = TSB.tsBridgeEither

instance TsBridgeBy tok a => TsBridgeVia tok (Maybe a) where
  tsBridgeVia = TSB.tsBridgeMaybe

instance (TsBridgeBy tok a, TsBridgeBy tok b) => TsBridgeVia tok (Tuple a b) where
  tsBridgeVia = TSB.tsBridgeTuple

instance TsBridgeVia tok Number where
  tsBridgeVia _ = TSB.tsBridgeNumber

instance (TSB.TsBridgeRecord tok r) => TsBridgeVia tok (Record r) where
  tsBridgeVia = TSB.tsBridgeRecord

instance (TSB.TsBridgeVariant tok r) => TsBridgeVia tok (Variant r) where
  tsBridgeVia = TSB.tsBridgeVariant

instance TsBridgeVia tok String where
  tsBridgeVia _ = TSB.tsBridgeString

instance (TsBridgeBy tok a) => TsBridgeVia tok (Array a) where
  tsBridgeVia = TSB.tsBridgeArray

instance (TsBridgeBy tok a) => TsBridgeVia tok (Object a) where
  tsBridgeVia = TSB.tsBridgeObject

instance (TsBridgeBy tok a) => TsBridgeVia tok (Effect a) where
  tsBridgeVia = TSB.tsBridgeEffect

instance (TsBridgeBy tok a) => TsBridgeVia tok (Nullable a) where
  tsBridgeVia = TSB.tsBridgeNullable

instance (TSB.GetName a, TsBridgeBy tok a, TsBridgeBy tok b) => TsBridgeVia tok (a -> b) where
  tsBridgeVia = TSB.tsBridgeFunction

else instance TsBridgeVia tok (Maybe a -> b) where
  tsBridgeVia = unsafeCoerce

instance (TSB.GetName a, TSB.GetName b, TSB.GetName c, TsBridgeBy tok a, TsBridgeBy tok b, TsBridgeBy tok c) => TsBridgeVia tok (Fn2 a b c) where
  tsBridgeVia = TSB.tsBridgeFn2

instance (TSB.GetName a, TSB.GetName b, TSB.GetName c, TsBridgeBy tok a, TsBridgeBy tok b, TsBridgeBy tok c, TsBridgeBy tok d) => TsBridgeVia tok (Fn3 a b c d) where
  tsBridgeVia = TSB.tsBridgeFn3

instance (TSB.GetName a, TSB.GetName b, TSB.GetName c, TSB.GetName d, TsBridgeBy tok a, TsBridgeBy tok b, TsBridgeBy tok c, TsBridgeBy tok d, TsBridgeBy tok e) => TsBridgeVia tok (Fn4 a b c d e) where
  tsBridgeVia = TSB.tsBridgeFn4

instance (TSB.GetName a, TsBridgeBy tok a, TsBridgeBy tok b) => TsBridgeVia tok (EffectFn1 a b) where
  tsBridgeVia = TSB.tsBridgeEffectFn1

instance (TSB.GetName a, TSB.GetName b, TSB.GetName c, TsBridgeBy tok a, TsBridgeBy tok b, TsBridgeBy tok c) => TsBridgeVia tok (EffectFn2 a b c) where
  tsBridgeVia = TSB.tsBridgeEffectFn2

instance (TSB.GetName a, TSB.GetName b, TSB.GetName c, TSB.GetName d, TsBridgeBy tok a, TsBridgeBy tok b, TsBridgeBy tok c, TsBridgeBy tok d) => TsBridgeVia tok (EffectFn3 a b c d) where
  tsBridgeVia = TSB.tsBridgeEffectFn3

instance (TSB.GetName a, TSB.GetName b, TSB.GetName c, TSB.GetName d, TSB.GetName e, TsBridgeBy tok a, TsBridgeBy tok b, TsBridgeBy tok c, TsBridgeBy tok d, TsBridgeBy tok e) => TsBridgeVia tok (EffectFn4 a b c d e) where
  tsBridgeVia = TSB.tsBridgeEffectFn4

instance TsBridgeBy tok a => TsBridgeVia tok (Promise a) where
  tsBridgeVia = TSB.tsBridgePromise

instance IsSymbol sym => TsBridgeVia tok (TSB.TypeVar sym) where
  tsBridgeVia _ = TSB.tsBridgeTypeVar

instance (TsBridgeBy tok a, TsBridgeBy tok b) => TsBridgeVia tok (OneOf a b) where
  tsBridgeVia = TSB.tsBridgeOneOf

instance (TsBridgeBy tok a, TsBridgeBy tok b) => TsBridgeVia tok (Intersection a b) where
  tsBridgeVia = tsBridgeIntersection

instance (TSB.TsBridgeVariantEncodedFlat tok symTag r) => TsBridgeVia tok (VariantEncodedFlat symTag r) where
  tsBridgeVia = TSB.tsBridgeVariantEncodedFlat

instance (TSB.TsBridgeVariantEncodedNested tok symTag symVal r) => TsBridgeVia tok (VariantEncodedNested symTag symVal r) where
  tsBridgeVia = TSB.tsBridgeVariantEncodedNested

instance TsBridgeVia tok Undefined where
  tsBridgeVia _ = TSB.tsBridgeUndefined

instance TsBridgeVia tok Null where
  tsBridgeVia _ = TSB.tsBridgeNull

instance (TSB.NTupleList tok as) => TsBridgeVia tok (NTuple as) where
  tsBridgeVia = TSB.tsBridgeNTuple

instance IsSymbol sym => TsBridgeVia tok (StringLit sym) where
  tsBridgeVia _ = TSB.tsBridgeStringLit

instance TsBridgeVia tok (BooleanLit "true") where
  tsBridgeVia _ = TSB.tsBridgeBooleanLitTrue

instance TsBridgeVia tok (BooleanLit "false") where
  tsBridgeVia _ = TSB.tsBridgeBooleanLitFalse

instance Reflectable h String => TsBridgeVia tok (Lit h String) where
  tsBridgeVia _ = TSB.tsBridgeStringLit'

instance Reflectable h Boolean => TsBridgeVia tok (Lit h Boolean) where
  tsBridgeVia _ = TSB.tsBridgeBooleanLit'

instance Reflectable h Int => TsBridgeVia tok (Lit h Int) where
  tsBridgeVia _ = TSB.tsBridgeIntLit'
