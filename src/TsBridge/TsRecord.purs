module TsBridge.TsRecord where

import Prelude

import DTS as DTS
import Data.Array as Array
import Data.Maybe (Maybe)
import Data.Reflectable (class Reflectable, reflectType)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Boolean (False, True)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record.Builder as RB
import TsBridge.Core (class TsBridgeBy, tsBridgeBy)
import TsBridge.Monad (TsBridgeM)
import Type.Data.Boolean (class If)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-------------------------------------------------------------------------------
--- Types
-------------------------------------------------------------------------------

foreign import data TsRecord :: Row (ModField Type) -> Type

type role TsRecord representational

foreign import data ModField :: Type -> Type

type role ModField representational

foreign import data Mod :: Row Boolean -> Type -> ModField Type

type role Mod phantom representational

-------------------------------------------------------------------------------
--- ToRecord
-------------------------------------------------------------------------------

class ToRecord rts r | rts -> r where
  toRecord :: TsRecord rts -> Record r

instance (ToRecordBuilder rl rts r, RowToList rts rl) => ToRecord rts r where
  toRecord x = RB.buildFromScratch $ toRecordBuilder prxRl x
    where
    prxRl = Proxy :: _ rl

---

class ToRecordBuilder :: RowList (ModField Type) -> Row (ModField Type) -> Row Type -> Constraint
class ToRecordBuilder rl rts r | rl rts -> r where
  toRecordBuilder :: forall proxy. proxy rl -> TsRecord rts -> RB.Builder {} { | r }

instance ToRecordBuilder RL.Nil rts () where
  toRecordBuilder _ _ = identity

instance
  ( ToRecordBuilder rl' rts r'
  , Row.Cons sym (Mod mods a) rtsx rts
  , Row.Cons sym a_ r' r
  , Row.Lacks sym r'
  , IsSymbol sym
  , Row.Cons "optional" required modsx mods
  , If required (Maybe a) a a_
  ) =>
  ToRecordBuilder (RL.Cons sym (Mod mods a) rl') rts r
  where
  toRecordBuilder _ x = buildHead <<< buildTail
    where
    buildHead = RB.insert prxSym (unsafeCoerce 1)
    buildTail = toRecordBuilder prxRl' x

    prxRl' = Proxy :: _ rl'
    prxSym = Proxy :: _ sym

-------------------------------------------------------------------------------
-- TsBridgeTsRecord
-------------------------------------------------------------------------------

class TsBridgeTsRecord :: Type -> Row (ModField Type) -> Constraint
class TsBridgeTsRecord tok r where
  tsBridgeTsRecord :: tok -> Proxy (TsRecord r) -> TsBridgeM DTS.TsType

instance (RowToList r rl, TsBridgeTsRecordRL tok rl) => TsBridgeTsRecord tok r where
  tsBridgeTsRecord tok _ = DTS.TsTypeRecord <$> tsBridgeTsRecordRL tok (Proxy :: _ rl)

---

class TsBridgeTsRecordRL :: Type -> RowList (ModField Type) -> Constraint
class TsBridgeTsRecordRL tok rl where
  tsBridgeTsRecordRL :: tok -> Proxy rl -> TsBridgeM (Array DTS.TsRecordField)

instance TsBridgeTsRecordRL tok RL.Nil where
  tsBridgeTsRecordRL _ _ = pure []

instance
  ( TsBridgeBy tok t
  , TsBridgeTsRecordRL tok rl
  , IsSymbol s
  , GetMods mods
  ) =>
  TsBridgeTsRecordRL tok (RL.Cons s (Mod mods t) rl) where
  tsBridgeTsRecordRL tok _ = do
    x <- tsBridgeBy tok (Proxy :: _ t)
    xs <- tsBridgeTsRecordRL tok (Proxy :: _ rl)
    let k = reflectSymbol (Proxy :: _ s)
    pure $
      Array.cons (DTS.TsRecordField k mods x) xs

    where
    mods = getMods (Proxy :: _ mods)

-------------------------------------------------------------------------------
--- Tests
-------------------------------------------------------------------------------

class GetMods :: Row Boolean -> Constraint
class GetMods r where
  getMods :: Proxy r -> DTS.PropModifiers

instance (RowToList r rl, GetModsRL rl) => GetMods r where
  getMods _ = getModsRL prxRl
    where
    prxRl = Proxy :: _ rl

---

class GetModsRL :: RowList Boolean -> Constraint
class GetModsRL rl where
  getModsRL :: Proxy rl -> DTS.PropModifiers

instance GetModsRL RL.Nil where
  getModsRL _ = { optional: false, readonly: false }

instance
  ( GetModsRL rl'
  , Reflectable optional Boolean
  ) =>
  GetModsRL (RL.Cons "optional" optional rl')
  where
  getModsRL _ = head tail
    where
    head = _ { optional = reflectType prxOptional }
    tail = getModsRL prxRl'

    prxRl' = Proxy :: _ rl'
    prxOptional = Proxy :: _ optional

instance
  ( GetModsRL rl'
  , Reflectable readonly Boolean
  ) =>
  GetModsRL (RL.Cons "readonly" readonly rl')
  where
  getModsRL _ = head tail
    where
    head = _ { readonly = reflectType prxReadonly }
    tail = getModsRL prxRl'

    prxRl' = Proxy :: _ rl'
    prxReadonly = Proxy :: _ readonly

-------------------------------------------------------------------------------
--- Tests
-------------------------------------------------------------------------------

test1
  :: TsRecord
       ( foo :: Mod (optional :: True) Int
       , bar :: Mod (optional :: True) Int
       , baz :: Mod (optional :: False) Int
       )
  -> Record
       ( foo :: Maybe Int
       , bar :: Maybe Int
       , baz :: Int
       )
test1 = toRecord

