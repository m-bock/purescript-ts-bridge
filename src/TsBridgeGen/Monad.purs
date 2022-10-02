module TsBridgeGen.Monad
  ( AppCapabalities(..)
  , AppEnv(..)
  , AppM
  , class MonadApp
  , class MonadLog
  , log
  , runAppM
  )
  where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, try)
import Control.Monad.Except (ExceptT, lift, runExceptT)
import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Writer (WriterT(..))
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Set (Set)
import Data.Typelevel.Undefined (undefined)
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, Error, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (error)
import Effect.Class.Console as E
import Node.Path (FilePath)
import Node.Process (exit)
import PureScript.CST (RecoveredParserResult(..), parseExpr)
import PureScript.CST.Types (Expr)
import Tidy (defaultFormatOptions, formatExpr)
import Tidy.Doc (FormatDoc(..))
import TsBridgeGen.Config (AppConfig)
import TsBridgeGen.Types (AppError, AppLog, AppWarning)

class
  ( MonadAsk (AppEnv m) m
  , MonadError AppError m
  , MonadLog AppLog m
  , MonadRec m
  ) <=
  MonadApp m

instance MonadApp AppM

newtype AppM a = AppM
  ( ReaderT (AppEnv AppM)
      (ExceptT AppError Aff)
      a
  )

  

newtype AppEnv m = AppEnv
  { config :: AppConfig
  , capabilities :: AppCapabalities m
  }

newtype AppCapabalities m = AppCapabalities
  { spawn :: String -> Array String -> m { stderr :: String, stdout :: String }
  , readTextFile :: FilePath -> m String
  , writeTextFile :: FilePath -> String -> m Unit
  , expandGlobsCwd :: Array String -> m (Set FilePath)
  , runPrettier :: String -> m String
  }

runAppM :: forall a. AppEnv AppM -> AppM a -> Effect Unit
runAppM env (AppM ma) = ma
  <#> const unit
  # flip runReaderT env
  # runExceptT
  # try
  >>= handleErrors >>> liftEffect
  # launchAff_

derive newtype instance MonadAff AppM
derive newtype instance MonadEffect AppM
derive newtype instance Bind AppM
derive newtype instance Monad AppM
derive newtype instance Apply AppM
derive newtype instance Applicative AppM
derive newtype instance MonadError AppError AppM
derive newtype instance MonadThrow AppError AppM
derive newtype instance Functor AppM
derive newtype instance MonadAsk (AppEnv AppM) AppM
derive newtype instance MonadRec AppM

instance MonadLog AppLog AppM where
  log = showPretty >>> E.log

-- instance MonadWarn AppWarning AppM where
--   warn = showPretty >>> E.warn
--   warnCount = pure 99

class Monad m <= MonadLog l m where
  log :: l -> m Unit

class  Monad m <= MonadErrorCount m where
  errorCount :: m Int


instance (Monoid w, MonadErrorCount m) => MonadErrorCount (WriterT w m ) where
  errorCount = errorCount # lift

instance (Monoid w, MonadLog l m) => MonadLog l (WriterT w m ) where
  log = log >>> lift

-- class Monad m <= MonadWarn w m | m -> w where
--   warn :: w -> m Unit
--   warnCount :: m Int

handleErrors :: forall a. Error \/ AppError \/ a -> Effect a
handleErrors = case _ of
  Left _ -> quitWithError "Unexpected Error"
  Right (Left appError) -> quitWithError $ printError appError
  Right (Right x) -> pure x

quitWithError :: forall a. String -> Effect a
quitWithError msg = do
  error msg
  exit 1

printError :: AppError -> String
printError = showPretty

showPretty :: forall a. Show a => a -> String
showPretty = show >>> parseExpr >>> case _ of
  ParseSucceeded m -> printExpr m
  _ -> "<invalid>"

printExpr :: Expr Void -> String
printExpr expr =
  formatExpr defaultFormatOptions expr
    # (\(FormatDoc { doc }) -> doc)
    # Dodo.print Dodo.plainText Dodo.twoSpaces