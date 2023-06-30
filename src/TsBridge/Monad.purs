module TsBridge.Monad
  ( Scope(..)
  , TsBridgeAccum(..)
  , TsBridgeM
  , getAccum
  , runTsBridgeM
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.State (StateT, get, modify_, runStateT)
import Control.Monad.Writer (class MonadTell, class MonadWriter)
import DTS (TsModuleFile, TsName)
import Data.Either (Either)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set.Ordered (OSet)
import Data.Set.Ordered as OSet
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import TsBridge.Types (AppError)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

newtype TsBridgeM a = TsBridgeM (StateT TsBridgeAccum (Except AppError) a)

newtype TsBridgeAccum = TsBridgeAccum
  { typeDefs :: Array TsModuleFile
  , scope :: Scope
  , registeredTypes :: Set { moduleName :: String, typeName :: String }
  }

newtype Scope = Scope
  { floating :: OSet TsName
  , fixed :: OSet TsName
  }

-------------------------------------------------------------------------------
-- Run
-------------------------------------------------------------------------------

runTsBridgeM :: forall a. TsBridgeM a -> Either AppError (a /\ TsBridgeAccum)
runTsBridgeM (TsBridgeM ma) = ma
  # flip runStateT mempty
  # runExcept

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

getAccum :: TsBridgeM TsBridgeAccum
getAccum = TsBridgeM get

derive newtype instance Semigroup TsBridgeAccum
derive newtype instance Semigroup Scope

derive instance Newtype TsBridgeAccum _
derive instance Newtype Scope _

derive newtype instance Monoid TsBridgeAccum

instance MonadTell TsBridgeAccum TsBridgeM where
  tell x = TsBridgeM do
    modify_ (_ <> x)

instance MonadWriter TsBridgeAccum TsBridgeM where
  listen (TsBridgeM ma) = TsBridgeM do
    x <- ma
    st <- get
    pure (Tuple x st)
  pass (TsBridgeM ma) = TsBridgeM do
    (x /\ f) <- ma
    modify_ f
    pure x

derive newtype instance Monad TsBridgeM

derive newtype instance Bind TsBridgeM

derive newtype instance Functor TsBridgeM

derive newtype instance Apply TsBridgeM

derive newtype instance Applicative TsBridgeM

derive newtype instance MonadThrow AppError TsBridgeM

derive newtype instance MonadError AppError TsBridgeM

instance Monoid Scope where
  mempty = Scope
    { floating: OSet.empty
    , fixed: OSet.empty
    }

