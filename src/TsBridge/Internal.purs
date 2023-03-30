module TsBridge.Internal where

import Prelude

import DTS as DTS
import TsBridge.Monad (TsBridgeM)

newtype StandaloneTsType = UnsafeStandaloneTsType (TsBridgeM DTS.TsType)

unStandaloneTsType :: StandaloneTsType -> (TsBridgeM DTS.TsType)
unStandaloneTsType (UnsafeStandaloneTsType x) = x