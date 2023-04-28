module Test.Util where

import Prelude

import Classless (class Init, class InitRecord, initRecord)
import Classless.EncodeJson as CEJ
import DTS (TsSource(..))
import DTS as DTS
import Data.Argonaut.Core (Json)
import Effect.Aff (Aff, error, throwError)

class AppEncodeJson a where
  appEncodeJson :: a -> Json

instance AppEncodeJson String where
  appEncodeJson = CEJ.string

instance AppEncodeJson TsSource where
  appEncodeJson (TsSource x) = appEncodeJson x

instance AppEncodeJson Int where
  appEncodeJson = CEJ.int

instance (AppEncodeJson a) => AppEncodeJson (Array a) where
  appEncodeJson = CEJ.array appEncodeJson

instance (CEJ.Record r' r, InitRecord MyInit r') => AppEncodeJson (Record r) where
  appEncodeJson = CEJ.record $ initRecord MyInit

data MyInit = MyInit

instance (AppEncodeJson a) => Init MyInit (a -> Json) where
  init _ = appEncodeJson

foreign import diff :: String -> String -> String

foreign import stringify :: Json -> String

shouldEqual :: forall a. Eq a => AppEncodeJson a => a -> a -> Aff Unit
shouldEqual x y
  | x == y = pure unit
  | otherwise = throwError $ error $ diff (stringify $ appEncodeJson x) (stringify $ appEncodeJson y)
