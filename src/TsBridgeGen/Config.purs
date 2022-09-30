module TsBridgeGen.Config
  ( AppConfig(..)
  , AppConfig_Mandatory
  , AppConfig_Optional
  , It
  , getConfig
  , runInitM
  ) where

import Prelude

import Control.Monad.Error.Class (liftEither, try)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Symbol (class IsSymbol)
import Data.Typelevel.Undefined (undefined)
import Effect (Effect)
import Effect.Aff (Aff, Error, runAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (error)
import Heterogeneous.Mapping (class HMapWithIndex, class MappingWithIndex, hmapWithIndex)
import Node.Process (exit, getEnv)
import Options.Applicative ((<**>))
import Options.Applicative as O
import Options.Applicative as Opt
import Options.Applicative.Types (optional)
import Prim.Row as Row
import Record as R
import Record as Record
import Record.Extra (sequenceRecord)
import Safe.Coerce (coerce)
import TsBridgeGen.Types (TsBridgeGenError(..))
import Type.Function (type ($))
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import TypedEnv as TypedEnv

type InitM a = ExceptT TsBridgeGenError Effect a

runInitM :: forall a. InitM a -> Effect a
runInitM ma = ma
  # runExceptT
  # try
  >>= handleErrors

handleErrors :: forall a. Error \/ TsBridgeGenError \/ a -> Effect a
handleErrors = case _ of
  Left _ -> quitWithError "Unexpected Error"
  Right (Left appError) -> quitWithError $ printError appError
  Right (Right x) -> pure x

quitWithError :: forall a. String -> Effect a
quitWithError msg = do
  error msg
  exit 1

printError :: TsBridgeGenError -> String
printError _ = "error"

--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------

newtype AppConfig = AppConfig
  { | AppConfig_Mandatory + AppConfig_Optional It + () }

type AppConfig_Mandatory r =
  ( assetsDir :: String
  | r
  )

type AppConfig_Optional :: forall k. (Type -> k) -> Row k -> Row k
type AppConfig_Optional maybe r =
  ( modulesFile :: maybe String
  , classFile :: maybe String
  | r
  )

defaults :: { | AppConfig_Optional It () }
defaults =
  { modulesFile: "src/MyApp/TsBridgeModules.purs"
  , classFile: "src/MyApp/TsBridgeClass.purs"
  }

--------------------------------------------------------------------------------
-- Cli Args
--------------------------------------------------------------------------------

newtype AppCliArgs = AppCliArgs
  { modulesFile :: Maybe String
  , classFile :: Maybe String
  }

optParser :: O.Parser AppCliArgs
optParser = AppCliArgs <$> sequenceRecord
  { classFile: cliOption "class-file" Nothing ".."
  , modulesFile: cliOption "modules-file" Nothing ".."
  }

getCliArgs :: InitM AppCliArgs
getCliArgs = liftEffect $ Opt.execParser opts
  where
  opts = Opt.info (optParser <**> Opt.helper)
    ( fold
        [ Opt.fullDesc
        , Opt.progDesc "Print a greeting for TARGET"
        , Opt.header "hello - a test for purescript-optparse"
        ]
    )

--------------------------------------------------------------------------------
-- Env Vars
--------------------------------------------------------------------------------

newtype AppEnvVars = AppEnvVars
  { | AppEnvVars_Spec TypedEnv.Resolved It }

type AppEnvVars_Spec :: forall k. (Symbol -> Type -> k) -> (Type -> Type) -> Row k
type AppEnvVars_Spec f wrap =
  ( assetsDir :: f "ASSETS_DIR" $ String
  )

getEnvVars :: InitM AppEnvVars
getEnvVars =
  getEnv
    <#> TypedEnv.fromEnv (Proxy :: _ (AppEnvVars_Spec TypedEnv.Variable Wrap))
    # liftEffect
    <#> lmap ErrParseEnvVars
    >>= liftEither
    <#> coerce >>> AppEnvVars

--------------------------------------------------------------------------------
-- 
--------------------------------------------------------------------------------

getConfig :: InitM AppConfig
getConfig = do
  cliArgs <- getCliArgs
  envVars <- getEnvVars
  pure $ mergeCfg cliArgs envVars

mergeCfg :: AppCliArgs -> AppEnvVars -> AppConfig
mergeCfg (AppCliArgs cli) (AppEnvVars env) =
  fromMaybeRecord defaults optional
    # R.union mandatory
    # AppConfig
  where
  optional =
    { modulesFile: cli.modulesFile
    , classFile: cli.classFile
    }

  mandatory =
    { assetsDir: env.assetsDir
    }

--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------
newtype Wrap a = Wrap a

type It :: forall k. k -> k
type It a = a

fromMaybeRecord :: forall r1 r2 r3. HMapWithIndex (MappingFromMaybeRecord r1) { | r2 } { | r3 } => { | r1 } -> { | r2 } -> { | r3 }
fromMaybeRecord = hmapWithIndex <<< MappingFromMaybeRecord

newtype MappingFromMaybeRecord r = MappingFromMaybeRecord { | r }

instance
  ( IsSymbol sym
  , Row.Cons sym a x r
  ) =>
  MappingWithIndex (MappingFromMaybeRecord r) (Proxy sym) (Maybe a) a where
  mappingWithIndex (MappingFromMaybeRecord rec) prop = fromMaybe (Record.get prop rec)

cliOption :: forall a. ReadM a => String -> Maybe Char -> String -> Opt.Parser (Maybe a)
cliOption long short help = (Opt.option $ optional $ readM) $ fold
  [ Opt.long long
  , maybe mempty Opt.short short
  , Opt.help help
  , Opt.value Nothing
  ]

class ReadM a where
  readM :: Opt.ReadM a

instance ReadM String where
  readM = Opt.str
