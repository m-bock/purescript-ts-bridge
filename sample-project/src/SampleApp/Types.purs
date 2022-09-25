module SampleApp.Types where

import Prelude

import TsBridge (class ToTsBridge, tsOpaqueType)

data AppState
  = On
  | Off
  | Loading Int

instance ToTsBridge AppState where
  toTsBridge = tsOpaqueType "SampleApp.Types" "AppState" []
