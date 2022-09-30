module TsBridgeGen.Monad
  ( TsBridgeGenError(..)
  , TsBridgeGenM
  , runTsBridgeGenM
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, try)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Data.Either (either)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff as JS
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Eff
import Node.Process as Process

data TsBridgeGenError
  = ErrSpawn String (Array String)
  | ErrParseModule
  | ErrReadFile String
  | ErrExpandGlobs

type TsBridgeGenEnv =
  { classFile :: String
  , modulesFile :: String
  }

newtype TsBridgeGenM a = TsBridgeGenM
  ( ReaderT TsBridgeGenEnv
      (ExceptT TsBridgeGenError Aff)
      a
  )

runTsBridgeGenM :: forall a. TsBridgeGenEnv -> TsBridgeGenM a -> Effect Unit
runTsBridgeGenM env (TsBridgeGenM ma) = ma
  <#> const unit
  # flip runReaderT env
  # runExceptT
  >>= either handleAppError pure
  # try
  >>= either handleNativeError pure
  # launchAff_

exit :: String -> Aff Unit
exit msg = do
  Eff.error msg
  liftEffect $ Process.exit 1

handleAppError :: TsBridgeGenError -> Aff Unit
handleAppError err = exit ("Error: " <> show err)

handleNativeError :: JS.Error -> Aff Unit
handleNativeError e = exit ("Unexpected Error: " <> JS.message e)

derive newtype instance MonadAff TsBridgeGenM
derive newtype instance MonadEffect TsBridgeGenM
derive newtype instance Bind TsBridgeGenM
derive newtype instance Monad TsBridgeGenM
derive newtype instance Apply TsBridgeGenM
derive newtype instance Applicative TsBridgeGenM
derive newtype instance MonadError TsBridgeGenError TsBridgeGenM
derive newtype instance MonadThrow TsBridgeGenError TsBridgeGenM
derive newtype instance Functor TsBridgeGenM
derive newtype instance MonadAsk TsBridgeGenEnv TsBridgeGenM

derive instance Generic TsBridgeGenError _

derive instance Eq TsBridgeGenError

instance Show TsBridgeGenError where
  show = genericShow
