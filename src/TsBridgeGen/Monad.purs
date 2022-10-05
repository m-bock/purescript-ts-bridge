module TsBridgeGen.Monad
  ( AppEffects(..)
  , AppEnv(..)
  , AppM
  , askAppConfig
  , askAppEffects
  , class MonadApp
  , class MonadAppConfig
  , class MonadAppEffects
  , class MonadLog
  , log
  , runAppM
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, try)
import Control.Monad.Except (class MonadTrans, ExceptT, lift, runExceptT)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Writer (WriterT)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Set (Set)
import Data.String (Pattern(..))
import Data.String as Str
import Data.Typelevel.Undefined (undefined)
import Dodo (Doc, indent, lines, text)
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, Error, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (error)
import Effect.Class.Console as E
import Heterogeneous.Mapping (class Mapping, hmap, mapping)
import Node.Path (FilePath)
import Node.Process (exit)
import PureScript.CST (RecoveredParserResult(..), parseExpr)
import Tidy (defaultFormatOptions, formatExpr)
import Tidy.Doc (FormatDoc(..))
import TsBridgeGen.Config (AppConfig(..))
import TsBridgeGen.Types (AppError(..), AppLog(..), Glob(..), SourcePosition(..))
import Type.Proxy (Proxy(..))

class
  ( MonadError AppError m
  , MonadLog AppLog m
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

instance (Monoid w, Monad m, MonadApp m) => MonadApp (WriterT w m)

class MonadAppConfig m where
  askAppConfig :: m AppConfig

instance MonadApp AppM

newtype AppM a = AppM
  ( ReaderT (AppEnv AppM)
      (ExceptT AppError Aff)
      a
  )

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

runAppM :: forall a. AppEnv AppM -> AppM a -> Effect Unit
runAppM env@(AppEnv { config }) (AppM ma) = ma
  <#> const unit
  # flip runReaderT env
  # runExceptT
  # try
  >>= handleErrors config >>> liftEffect
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
  log x = do
    printAppLog x
      <#> Dodo.print Dodo.plainText Dodo.twoSpaces
      >>= E.log

printAppLog :: forall m a. MonadApp m => AppLog -> m (Doc a)
printAppLog x = do
  config@(AppConfig { debug }) <- askAppConfig
  c <- pure 0 -- errorCount

  pure
    if debug then showDoc x
    else case x of
      LogLiteral str -> str # Str.split (Pattern "\n") <#> text # lines
      LogError e -> lines
        [ text ("Error " <> show (c + 1) <> ":")
        , indent $ printError config e
        ]

printError :: forall a. AppConfig -> AppError -> Doc a
printError config@(AppConfig { debug }) x =
  if debug then showDoc x
  else case x of
    ErrSpawn cmd _ ->
      text ("Failed to spawn Command " <> cmd)
    ErrParseModule ->
      text ("Failed to parse PureScript module")
    ErrReadFile path ->
      text ("Failed to read from file " <> path)
    ErrWriteFile path ->
      text ("Failed to write to file " <> path)
    ErrExpandGlobs ->
      text "Failed to expand globs"
    ErrParseEnvVars _ ->
      text "Failed to parse environment variables"
    ErrLiteral str -> str
      # Str.split (Pattern "\n")
      <#> text
      # lines
    ErrParseToJson _ ->
      text "Found invalid JSON"
    ErrParseToData _ ->
      text "JSON config does not have the right shape"
    ErrUnknown ->
      text "An unknown error occured. Try DEBUG=true" --
    AtFilePosition fp p e -> lines
      [ printError config e
      , text ("at " <> printPos fp p)
      ]

printPos :: FilePath -> SourcePosition -> String
printPos fp (SourcePosition { line, column }) = fp
  <> ":"
  <> show line
  <> ":"
  <> show column

class Monad m <= MonadLog l m where
  log :: l -> m Unit

class Monad m <= MonadErrorCount m where
  errorCount :: m Int

instance (Monoid w, MonadErrorCount m) => MonadErrorCount (WriterT w m) where
  errorCount = errorCount # lift

instance (Monoid w, MonadLog l m) => MonadLog l (WriterT w m) where
  log = log >>> lift

handleErrors :: forall a. AppConfig -> Error \/ AppError \/ a -> Effect a
handleErrors config@(AppConfig { debug }) = case _ of
  Left err ->
    if debug then
      quitWithError ("Unexpected Error.\n" <> show err)
    else
      quitWithError "Unexpected Error. Try to set DEBUG=true"
  Right (Left appError) -> quitWithError $ Dodo.print Dodo.plainText Dodo.twoSpaces $ printError config appError
  Right (Right x) -> pure x

quitWithError :: forall a. String -> Effect a
quitWithError msg = do
  error msg
  exit 1

showDoc :: forall a b. Show a => a -> Doc b
showDoc = show >>> parseExpr >>> case _ of
  ParseSucceeded expr -> formatExpr defaultFormatOptions expr
    # (\(FormatDoc { doc }) -> doc)
  _ -> text "<invalid>"

