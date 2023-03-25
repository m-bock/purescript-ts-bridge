module TsBridge.Monad
  ( Scope(..)
  , TsBridgeAccum(..)
  , TsBridgeM
  , runTsBridgeM
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.Writer (class MonadTell, class MonadWriter, WriterT, runWriterT)
import Data.Either (Either)
import Data.Newtype (class Newtype)
import Data.Set.Ordered (OSet)
import Data.Set.Ordered as OSet
import Data.Tuple.Nested (type (/\))
import TsBridge.DTS (TsModuleFile, TsName, Error)

-------------------------------------------------------------------------------
-- Types / TsBridge
-------------------------------------------------------------------------------

newtype TsBridgeM a = TsBridgeM (WriterT TsBridgeAccum (Except Error) a)

derive newtype instance MonadThrow Error TsBridgeM

derive newtype instance MonadError Error TsBridgeM

newtype TsBridgeAccum = TsBridgeAccum
  { typeDefs :: Array TsModuleFile
  , scope :: Scope
  }

newtype Scope = Scope
  { floating :: OSet TsName
  , fixed :: OSet TsName
  }

derive newtype instance Semigroup Scope

instance Monoid Scope where
  mempty = Scope
    { floating: OSet.empty
    , fixed: OSet.empty
    }

derive instance Newtype Scope _

runTsBridgeM :: forall a. TsBridgeM a -> Either Error (a /\ TsBridgeAccum)
runTsBridgeM (TsBridgeM ma) = ma
  # runWriterT
  # runExcept

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

