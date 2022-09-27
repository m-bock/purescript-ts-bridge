module TsBridgeGen.Monad where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, try)
import Control.Monad.Except (ExceptT, runExceptT)
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

derive instance Generic TsBridgeGenError _

instance Show TsBridgeGenError where
  show = genericShow

newtype TsBridgeGenM a = TsBridgeGenM (ExceptT TsBridgeGenError Aff a)

runTsBridgeGenM :: forall a. TsBridgeGenM a -> Effect Unit
runTsBridgeGenM (TsBridgeGenM ma) = ma
  <#> const unit
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
