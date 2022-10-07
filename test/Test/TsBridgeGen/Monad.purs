module Test.TsBridgeGen.Monad
  ( TestM
  , TestMAccum
  , TestMResult(..)
  , TestState
  , defaultTestCapabilities
  , defaultTestConfig
  , runTestM
  , runTestM_
  )
  where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.RWS (RWS, RWSResult(..), ask, runRWS, tell)
import Control.Monad.Reader (class MonadAsk)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Show.Generic (genericShow)
import Data.Typelevel.Undefined (undefined)
import TsBridgeGen.Config (AppConfig(..))
import TsBridgeGen.Monad (class MonadApp, class MonadAppConfig, class MonadAppEffects, class MonadLog, class MonadMultipleErrors, AppEffects(..), AppEnv(..), pushError)
import TsBridgeGen.Types (AppError(..), AppLog)

type TestState = Map String String

type TestMAccum = { logs :: Array AppLog, errors :: Array AppError }

emptyTestMAccum :: TestMAccum
emptyTestMAccum = mempty

newtype TestM a = TestM
  (ExceptT AppError (RWS (AppEnv TestM) TestMAccum TestState) a)

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
  log l = TestM do
    tell emptyTestMAccum { logs = pure l }
  getLogs = undefined

instance MonadMultipleErrors AppError TestM where
  pushError l = TestM do
    tell emptyTestMAccum { errors = pure l }
    
-- getLogs = undefined

instance MonadApp TestM

instance MonadAppEffects TestM where
  askAppEffects = do
    (AppEnv { capabilities }) <- ask
    pure capabilities

instance MonadAppConfig TestM where
  askAppConfig = do
    (AppEnv { config }) <- ask
    pure config

data TestMResult a = TestMResult TestState TestMAccum (Either AppError a)

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

defaultTestCapabilities :: AppEffects TestM
defaultTestCapabilities = AppEffects
  { readTextFile: \_ -> throwError ErrUnknown
  , writeTextFile: \_ _ -> throwError ErrUnknown
  , expandGlobsCwd: \_ -> throwError ErrUnknown
  , runPrettier: \x -> pure x --throwError ErrUnknown
  , mkdirRec: \_ -> throwError ErrUnknown
  , spagoLsDepsTransitive: throwError ErrUnknown
  , spagoSources: throwError ErrUnknown
  }

defaultTestConfig :: AppConfig
defaultTestConfig = AppConfig
  { assetsDir: ""
  , modulesFile: ""
  , classFile: ""
  , spagoFile: ""
  , packagesFile: ""
  , allDepsFile: ""
  , debug: false
  }

initState :: TestState
initState = Map.empty

runTestM_ :: forall a. AppEnv TestM -> TestM a -> TestMResult a
runTestM_ testEnv = runTestM testEnv initState