module TsBridge.ABC where

import Prelude

import Control.Monad.Writer (tell)
import Data.Set.Ordered as OSet
import Safe.Coerce (coerce)
import TsBridge (class ToTsBridge, TsName(..))
import TsBridge.DTS (TsType(..))
import TsBridge.Monad (TsBridgeAccum(..), TsBridgeM)
import TsBridge.Monad as TsBridge.Monad

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data A = A

data B = B

data C = C

data D = D

data E = E

data F = F

data G = G

data H = H

data I = I

data J = J

data K = K

data L = L

data M = M

data N = N

data O = O

data P = P

data Q = Q

data R = R

data S = S

data T = T

data U = U

data V = V

data W = W

data X = X

data Y = Y

data Z = Z

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance ToTsBridge A where
  toTsBridge _ = tsTypeVar "A"

instance ToTsBridge B where
  toTsBridge _ = tsTypeVar "B"

instance ToTsBridge C where
  toTsBridge _ = tsTypeVar "C"

instance ToTsBridge D where
  toTsBridge _ = tsTypeVar "D"

instance ToTsBridge E where
  toTsBridge _ = tsTypeVar "E"

instance ToTsBridge F where
  toTsBridge _ = tsTypeVar "F"

instance ToTsBridge G where
  toTsBridge _ = tsTypeVar "G"

instance ToTsBridge H where
  toTsBridge _ = tsTypeVar "H"

instance ToTsBridge I where
  toTsBridge _ = tsTypeVar "I"

instance ToTsBridge J where
  toTsBridge _ = tsTypeVar "J"

instance ToTsBridge K where
  toTsBridge _ = tsTypeVar "K"

instance ToTsBridge L where
  toTsBridge _ = tsTypeVar "L"

instance ToTsBridge M where
  toTsBridge _ = tsTypeVar "M"

instance ToTsBridge N where
  toTsBridge _ = tsTypeVar "N"

instance ToTsBridge O where
  toTsBridge _ = tsTypeVar "O"

instance ToTsBridge P where
  toTsBridge _ = tsTypeVar "P"

instance ToTsBridge Q where
  toTsBridge _ = tsTypeVar "Q"

instance ToTsBridge R where
  toTsBridge _ = tsTypeVar "R"

instance ToTsBridge S where
  toTsBridge _ = tsTypeVar "S"

instance ToTsBridge T where
  toTsBridge _ = tsTypeVar "T"

instance ToTsBridge U where
  toTsBridge _ = tsTypeVar "U"

instance ToTsBridge V where
  toTsBridge _ = tsTypeVar "V"

instance ToTsBridge W where
  toTsBridge _ = tsTypeVar "W"

instance ToTsBridge X where
  toTsBridge _ = tsTypeVar "X"

instance ToTsBridge Y where
  toTsBridge _ = tsTypeVar "Y"

instance ToTsBridge Z where
  toTsBridge _ = tsTypeVar "Z"

-------------------------------------------------------------------------------
-- Util
-------------------------------------------------------------------------------

tsTypeVar :: String -> TsBridgeM TsType
tsTypeVar x = do
  let tsName = TsName x
  tell
    $ TsBridgeAccum
    $
      { typeDefs: mempty
      , imports: mempty
      , scope:
          { floating: coerce $ OSet.singleton tsName
          , fixed: mempty
          }
      }
  pure $ TsTypeVar tsName
