module TsBridge.Monad
  ( Scope(..)
  , TsBridgeAccum(..)
  , TsBridgeError(..)
  , TsBridgeM
  , printTsBridgeError
  , runTsBridgeM
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.Writer (class MonadTell, class MonadWriter, WriterT, runWriterT)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set.Ordered (OSet)
import Data.Set.Ordered as OSet
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\))
import TsBridge.DTS (TsModuleFile, TsName)

-------------------------------------------------------------------------------
-- Types / TsBridge
-------------------------------------------------------------------------------

newtype TsBridgeM a = TsBridgeM (WriterT TsBridgeAccum (Except TsBridgeError) a)

derive newtype instance MonadThrow TsBridgeError TsBridgeM

derive newtype instance MonadError TsBridgeError TsBridgeM

data TsBridgeError
  = ErrUnquantifiedTypeVariables (Set TsName)
  | ErrIllegalSymbolName String
  | AtModule String TsBridgeError

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

runTsBridgeM :: forall a. TsBridgeM a -> Either TsBridgeError (a /\ TsBridgeAccum)
runTsBridgeM (TsBridgeM ma) = ma
  # runWriterT
  # runExcept

printTsBridgeError :: TsBridgeError -> String
printTsBridgeError _ = ""

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

derive instance Generic TsBridgeError _

derive instance Eq TsBridgeError

instance Show TsBridgeError where
  show x = genericShow x