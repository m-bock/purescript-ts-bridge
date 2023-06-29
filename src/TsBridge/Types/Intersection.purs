module TsBridge.Types.Intersection
  ( Intersection
  , class ToTuples
  , fst
  , snd
  , toTuple
  , tsBridgeIntersection
  , type (|&|)
  , toTuples
  ) where

import Prelude

import DTS as DTS
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))
import TsBridge.Monad (TsBridgeM)
import TsBridge.Core (class TsBridgeBy, tsBridgeBy)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

foreign import data Intersection :: Type -> Type -> Type

infixr 7 type Intersection as |&|

fst :: forall a b. Intersection a b -> a
fst = unsafeCoerce

snd :: forall a b. Intersection a b -> b
snd = unsafeCoerce

toTuple :: forall a b. Intersection a b -> Tuple a b
toTuple is = Tuple (fst is) (snd is)

-------------------------------------------------------------------------------

tsBridgeIntersection
  :: forall a b tok
   . TsBridgeBy tok a
  => TsBridgeBy tok b
  => tok
  -> Proxy (Intersection a b)
  -> TsBridgeM DTS.TsType
tsBridgeIntersection tok _ = do
  x <- tsBridgeBy tok (Proxy :: _ a)
  y <- tsBridgeBy tok (Proxy :: _ b)

  pure $ DTS.TsTypeIntersection [ x, y ]

-------------------------------------------------------------------------------
--- ToTuples
-------------------------------------------------------------------------------

class ToTuples a b | a -> b where
  toTuples :: a -> b

instance ToTuples b b' => ToTuples (Intersection a b) (Tuple a b') where
  toTuples its = Tuple (fst its) $ toTuples (snd its)

else instance ToTuples a a where
  toTuples = identity

test1 :: Int |&| String |&| Boolean |&| Char -> Int /\ String /\ Boolean /\ Char
test1 = toTuples