module TsBridge.Monad
  ( Scope
  , TsBridgeAccum(..)
  , TsBridgeM(..)
  , TsBridge_Monad_Wrap(..)
  , defaultTsBridgeAccum
  , runTsBridgeM
  ) where

import Prelude

import Control.Monad.Writer (class MonadTell, class MonadWriter, Writer, runWriter)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set.Ordered (OSet)
import Data.Set.Ordered as OSet
import Data.Tuple.Nested (type (/\))
import TsBridge.DTS (TsImport, TsModuleFile, TsName)

-------------------------------------------------------------------------------
-- Types / TsBridge
-------------------------------------------------------------------------------

newtype TsBridgeM a = TsBridgeM (Writer TsBridgeAccum a)

newtype TsBridgeAccum = TsBridgeAccum
  { typeDefs :: Array TsModuleFile
  , imports :: Set TsImport
  , scope :: Scope
  }

defaultTsBridgeAccum :: TsBridgeAccum
defaultTsBridgeAccum = TsBridgeAccum
  { typeDefs: mempty
  , imports: mempty
  , scope: mempty
  }

type Scope =
  { floating :: TsBridge_Monad_Wrap (OSet TsName)
  , fixed :: TsBridge_Monad_Wrap (OSet TsName)
  }

runTsBridgeM :: forall a. TsBridgeM a -> a /\ TsBridgeAccum
runTsBridgeM (TsBridgeM ma) = runWriter ma

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

derive instance Newtype TsBridgeAccum _

derive newtype instance Monoid TsBridgeAccum

derive newtype instance Semigroup TsBridgeAccum

derive newtype instance MonadTell TsBridgeAccum TsBridgeM

derive newtype instance MonadWriter TsBridgeAccum TsBridgeM

derive newtype instance Monad TsBridgeM

derive newtype instance Bind TsBridgeM

derive newtype instance Functor TsBridgeM

derive newtype instance Apply TsBridgeM

derive newtype instance Applicative TsBridgeM

-------------------------------------------------------------------------------
-- Wrap
-------------------------------------------------------------------------------

newtype TsBridge_Monad_Wrap a = TsBridge_Monad_Wrap a

derive instance Newtype (TsBridge_Monad_Wrap a) _

derive newtype instance Eq a => Semigroup (TsBridge_Monad_Wrap (OSet a))

instance Eq a => Monoid (TsBridge_Monad_Wrap (OSet a)) where
  mempty = TsBridge_Monad_Wrap $ OSet.empty