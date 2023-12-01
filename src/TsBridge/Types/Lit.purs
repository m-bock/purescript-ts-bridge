module TsBridge.Types.Lit
  ( Lit
  , unLit
  , mkLit
  ) where

import Prelude

import Data.Reflectable (class Reflectable, reflectType)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Untagged.TypeCheck (class HasRuntimeType, hasRuntimeType)

newtype Lit :: forall k. k -> Type -> Type
newtype Lit h l = Lit l

mkLit :: forall @h l. Reflectable h l => Lit h l
mkLit = Lit (reflectType @h Proxy :: l)

unLit :: forall h l. Lit h l -> l
unLit (Lit val) = val

instance
  ( Reflectable h l
  , HasRuntimeType l
  , Eq l
  ) =>
  HasRuntimeType (Lit h l) where
  hasRuntimeType _ for =
    if hasRuntimeType (Proxy :: _ l) for then
      unsafeCoerce for == reflectType @h Proxy
    else false