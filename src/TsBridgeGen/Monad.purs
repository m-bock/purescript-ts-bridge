module TsBridgeGen.Monad
  ( AppEffects(..)
  , AppEnv(..)
  , AppM
  , MonadAppAccum
  , askAppConfig
  , askAppEffects
  , class MonadApp
  , class MonadAppConfig
  , class MonadAppEffects
  , class MonadLog
  , class MonadMultipleErrors
  , getLogs
  , log
  , pushError
  , runAppM
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, try)
import Control.Monad.Except (class MonadTrans, ExceptT, lift, runExceptT)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Writer (class MonadWriter, WriterT(..), Writer, listen, listens, runWriterT, tell)
import Data.Argonaut (printJsonDecodeError)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Set (Set)
import Data.String (Pattern(..))
import Data.String as Str
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Undefined (undefined)
import Dodo (Doc, indent, lines, text)
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, Error, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (error)
import Node.Path (FilePath)
import Node.Process (exit)
import PureScript.CST (RecoveredParserResult(..), parseExpr)
import Tidy (defaultFormatOptions, formatExpr)
import Tidy.Doc (FormatDoc(..))
import TsBridgeGen.Config (AppConfig(..))
import TsBridgeGen.Types (AppError(..), AppLog(..), Glob, SourcePosition(..))

class
  ( MonadError AppError m
  , MonadLog AppLog m
  , MonadMultipleErrors AppError m
  , MonadRec m
  , MonadAppEffects m
  , MonadAppConfig m
  ) <=
  MonadApp m

instance MonadAppEffects AppM where
  askAppEffects = do
    (AppEnv { capabilities }) <- ask
    pure capabilities

instance MonadAppConfig AppM where
  askAppConfig = do
    (AppEnv { config }) <- ask
    pure config

class MonadAppEffects m where
  askAppEffects :: m (AppEffects m)

instance (Monoid w, Monad m, MonadAppEffects m) => MonadAppEffects (WriterT w m) where
  askAppEffects = lift $ liftAppEffects <$> askAppEffects

instance (Monoid w, Monad m, MonadAppConfig m) => MonadAppConfig (WriterT w m) where
  askAppConfig = lift $ askAppConfig

-- instance (Monoid w, Monad m, MonadApp m) => MonadApp (WriterT w m)

derive newtype instance MonadWriter AppMAccum AppM

instance (Monoid w, MonadApp m) => MonadApp (WriterT w m)

instance (Monoid w, MonadLog l m) => MonadLog l (WriterT w m)
  where
  log = log >>> lift
  getLogs (WriterT ma) = WriterT do
    logs <- getLogs ma
    Tuple _ w <- ma
    pure (Tuple logs w)

instance (Monoid w, MonadMultipleErrors e m) => MonadMultipleErrors e (WriterT w m)
  where
  pushError = pushError >>> lift

class MonadAppConfig m where
  askAppConfig :: m AppConfig

instance MonadApp AppM

newtype AppM a = AppM
  ( ExceptT AppError
      ( ReaderT (AppEnv AppM)
          ( WriterT AppMAccum
              Aff
          )
      )
      a
  )

type AppMAccum =
  { errors :: Array AppError
  , logs :: Array AppLog
  }

newtype AppEnv m = AppEnv
  { config :: AppConfig
  , capabilities :: AppEffects m
  }

liftAppEffects :: forall t m. Monad m => MonadTrans t => AppEffects m -> AppEffects (t m)
liftAppEffects (AppEffects r) = AppEffects
  { mkdirRec: \x1 -> r.mkdirRec x1 # lift
  , readTextFile: \x1 -> r.readTextFile x1 # lift
  , writeTextFile: \x1 x2 -> r.writeTextFile x1 x2 # lift
  , expandGlobsCwd: \x1 -> r.expandGlobsCwd x1 # lift
  , runPrettier: \x1 -> r.runPrettier x1 # lift
  , spagoLsDepsTransitive: lift r.spagoLsDepsTransitive
  , spagoSources: lift r.spagoSources
  }

newtype AppEffects m = AppEffects
  { mkdirRec :: FilePath -> m Unit
  , readTextFile :: FilePath -> m String
  , writeTextFile :: FilePath -> String -> m Unit
  , expandGlobsCwd :: Array String -> m (Set FilePath)
  , runPrettier :: String -> m String
  , spagoLsDepsTransitive :: m (Array { packageName :: String })
  , spagoSources :: m (Array Glob)
  }

type MonadAppAccum =
  { logs :: Array AppLog
  , errors :: Array AppError
  }

runAppM :: forall a. AppEnv AppM -> AppM a -> Aff (Tuple (Either Error (Either AppError a)) MonadAppAccum)
runAppM env (AppM ma) = ma
  # runExceptT
  # try
  # flip runReaderT env
  # runWriterT

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
  log l = AppM $ tell $ emptyAppMAccum { logs = pure l }
  getLogs (AppM ma) = ma # listens _.logs <#> snd # AppM

emptyAppMAccum :: AppMAccum
emptyAppMAccum = mempty

instance MonadMultipleErrors AppError AppM where
  pushError error = AppM $ tell $ emptyAppMAccum { errors = pure error }

--  getErrors (AppM ma) = ma # listens _.errors # AppM

printPos :: FilePath -> SourcePosition -> String
printPos fp (SourcePosition { line, column }) = fp
  <> ":"
  <> show (line + 1)
  <> ":"
  <> show (column + 1)

class Monad m <= MonadLog l m where
  log :: l -> m Unit
  getLogs :: forall a. m a -> m (Array l)

class Monad m <= MonadMultipleErrors e m where
  pushError :: e -> m Unit

--getErrors :: forall a. m a -> m (Tuple a (Array e))

-- instance
--   ( Monoid w
--   , MonadMultipleErrors e m
--   ) =>
--   MonadMultipleErrors e (WriterT w m) where
--   pushError = pushError >>> lift
--   getErrors (WriterT ma) = WriterT do
--     Tuple a w <- getErrors ma
--     pure undefined -- $ Tuple (Tuple a w) w

-- instance (Monoid w, MonadLog l m) => MonadLog l (WriterT w m) where
--   log = log >>> lift
--   getLogs (WriterT ma) = undefined -- getLogs ma # WriterT

