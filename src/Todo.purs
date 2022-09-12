module Todo where

import Data.Typelevel.Undefined (undefined)
import Prim.TypeError (class Warn, Text)

todo :: Warn (Text "TODO") => forall a. a
todo = undefined