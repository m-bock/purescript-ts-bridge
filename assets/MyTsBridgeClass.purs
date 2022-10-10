module MyTsBridgeClass where

{-GEN:imports
{}
-}

import TypedEnv as Auto.TypedEnv
import Data.Either (Either)
import Data.Typelevel.Undefined (undefined)
import Effect (Effect)
import Prelude
import Prim.RowList (class RowToList)
import TsBridge (class GenRecord, class Mapping, TsBridgeM, TsType, defaultArray, defaultBoolean, defaultEffect, defaultNewtype, defaultOpaqueType, defaultProxy, defaultRecord, defaultString, defaultUnit, tsOpaqueType)
import TsBridge.ABC (A(..), B(..), C(..), D(..), E(..), F(..), G(..), H(..), I(..), J(..), K(..), L(..), M(..), N(..), O(..), P(..), Q(..), R(..), S(..), T(..), U(..), V(..), W(..), X(..), Y(..), Z(..))
import Type.Proxy (Proxy(..))

{-GEN:END-}
class ToTsBridge a where
  toTsBridge :: a -> TsBridgeM TsType

data MappingToTsBridge = Mp

instance ToTsBridge a => Mapping MappingToTsBridge a (TsBridgeM TsType) where
  mapping _ = toTsBridge

instance ToTsBridge a => ToTsBridge (Proxy a) where
  toTsBridge = defaultProxy Mp

instance ToTsBridge String where
  toTsBridge = defaultString

instance ToTsBridge Boolean where
  toTsBridge = defaultBoolean

instance ToTsBridge a => ToTsBridge (Effect a) where
  toTsBridge = defaultEffect Mp

instance ToTsBridge a => ToTsBridge (Array a) where
  toTsBridge = defaultArray Mp

instance ToTsBridge Int where
  toTsBridge = undefined

instance ToTsBridge Unit where
  toTsBridge = defaultUnit

instance
  ( GenRecord MappingToTsBridge rl
  , RowToList r rl
  ) =>
  ToTsBridge (Record r) where
  toTsBridge = defaultRecord Mp


{-GEN:instances
{ "include": [ "**" ]
, "exclude": []
}
-}

{-GEN:END-}