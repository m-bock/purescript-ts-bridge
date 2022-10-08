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
  , censorLogs
  , censorErrors
  , log
  , pushError
  , runAppM
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, try)
import Control.Monad.Except (class MonadTrans, ExceptT, lift, runExceptT)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Writer (class MonadWriter, WriterT(..), censor, runWriterT, tell)
import Data.Either (Either)
import Data.Lens (over)
import Data.Lens.Record (prop)
import Data.Set (Set)
import Data.Tuple (Tuple)
import Effect.Aff (Aff, Error)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Node.Path (FilePath)
import TsBridgeGen.Config (AppConfig)
import TsBridgeGen.Types (AppError, AppLog, Glob, SourcePosition(..))
import Type.Proxy (Proxy(..))

class
  ( MonadError AppError m
  , MonadLog AppLog m
  , MonadMultipleErrors AppError m
  , MonadRec m
  , MonadAppEffects m
  , MonadAppConfig m
  ) <=
  MonadApp m

class Monad m <= MonadLog l m where
  log :: l -> m Unit
  censorLogs :: forall a. (Array l -> Array l) -> m a -> m a

class Monad m <= MonadMultipleErrors e m where
  pushError :: e -> m Unit
  censorErrors :: forall a. (Array e -> Array e) -> m a -> m a

class MonadAppConfig m where
  askAppConfig :: m AppConfig

class MonadAppEffects m where
  askAppEffects :: m (AppEffects m)

instance (Monoid w, Monad m, MonadAppEffects m) => MonadAppEffects (WriterT w m) where
  askAppEffects = lift $ liftAppEffects <$> askAppEffects

instance (Monoid w, Monad m, MonadAppConfig m) => MonadAppConfig (WriterT w m) where
  askAppConfig = lift $ askAppConfig

-- instance (Monoid w, Monad m, MonadApp m) => MonadApp (WriterT w m)

instance (Monoid w, MonadApp m) => MonadApp (WriterT w m)

instance (Monoid w, MonadLog l m) => MonadLog l (WriterT w m)
  where
  log = log >>> lift
  censorLogs f (WriterT ma) = WriterT $ censorLogs f ma

instance (Monoid w, MonadMultipleErrors e m) => MonadMultipleErrors e (WriterT w m)
  where
  pushError = pushError >>> lift
  censorErrors f (WriterT ma) = WriterT $ censorErrors f ma

type AppMAccum =
  { errors :: Array AppError
  , logs :: Array AppLog
  }

newtype AppEnv m = AppEnv
  { config :: AppConfig
  , capabilities :: AppEffects m
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

emptyAppMAccum :: AppMAccum
emptyAppMAccum = mempty

printPos :: FilePath -> SourcePosition -> String
printPos fp (SourcePosition { line, column }) = fp
  <> ":"
  <> show (line + 1)
  <> ":"
  <> show (column + 1)

newtype AppM a = AppM
  ( ExceptT AppError
      ( ReaderT (AppEnv AppM)
          ( WriterT AppMAccum
              Aff
          )
      )
      a
  )

instance MonadAppEffects AppM where
  askAppEffects = do
    (AppEnv { capabilities }) <- ask
    pure capabilities

instance MonadAppConfig AppM where
  askAppConfig = do
    (AppEnv { config }) <- ask
    pure config

instance MonadLog AppLog AppM where
  log l = AppM $ tell $ emptyAppMAccum { logs = pure l }
  censorLogs f (AppM ma) = ma # censor (over (prop (Proxy :: _ "logs")) f) # AppM

instance MonadMultipleErrors AppError AppM where
  pushError error = AppM $ tell $ emptyAppMAccum { errors = pure error }
  censorErrors f (AppM ma) = ma # censor (over (prop (Proxy :: _ "errors")) f) # AppM

instance MonadApp AppM

derive newtype instance MonadWriter AppMAccum AppM
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