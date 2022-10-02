module Test.TsBridgeGen.Monad
  ( AppLogs
  , TestM
  , TestMResult(..)
  , TestState
  , runTestM
  , runTestM_
  )
  where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.RWS (RWS, RWSResult(..), runRWS)
import Control.Monad.Reader (class MonadAsk)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Typelevel.Undefined (undefined)
import TsBridgeGen (class MonadApp, class MonadLog, AppEnv, AppError, AppLog)

type TestState = {}

initState :: TestState
initState = {}

type AppLogs = Array AppLog

newtype TestM a = TestM
  (ExceptT AppError (RWS (AppEnv TestM) AppLogs TestState) a)

derive newtype instance Bind TestM
derive newtype instance Monad TestM
derive newtype instance Apply TestM
derive newtype instance Applicative TestM
derive newtype instance Functor TestM
derive newtype instance MonadRec TestM
derive newtype instance MonadError AppError TestM
derive newtype instance MonadThrow AppError TestM
derive newtype instance MonadAsk (AppEnv TestM) TestM

instance MonadLog AppLog TestM where
  log = undefined

instance MonadApp TestM

data TestMResult a = TestMResult TestState AppLogs (Either AppError a)

derive instance Functor TestMResult

derive instance Generic (TestMResult a) _

derive instance Eq a => Eq (TestMResult a)

instance Show a => Show (TestMResult a) where
  show = genericShow

runTestM :: forall a. AppEnv TestM -> TestState -> TestM a -> TestMResult a
runTestM r s (TestM ma) = ma
  # runExceptT
  # (\x -> runRWS x r s)
  # (\(RWSResult s' x w) -> TestMResult s' w x)

testEnv :: AppEnv TestM
testEnv = undefined

runTestM_ :: forall a. TestM a -> TestMResult a
runTestM_ = runTestM testEnv initState