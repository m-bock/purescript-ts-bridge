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
import Data.Array (uncons)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..))
import Data.String as Str
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested ((/\))
import Dodo as Dodo
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (error)
import Effect.Exception (Error)
import Heterogeneous.Mapping (class HMapWithIndex, class MappingWithIndex, hmapWithIndex)
import Node.Process (exit, getEnv, lookupEnv)
import Options.Applicative ((<**>))
import Options.Applicative as O
import Options.Applicative as Opt
import Options.Applicative.Help (extractChunk, stringChunk, tabulate, vcatChunks)
import Options.Applicative.Types (optional)
import Prim.Row as Row
import PureScript.CST (RecoveredParserResult(..), parseExpr)
import PureScript.CST.Types (Expr)
import Record as R
import Record as Record
import Record.Extra (sequenceRecord)
import Safe.Coerce (coerce)
import Tidy (defaultFormatOptions, formatExpr)
import Tidy.Doc (FormatDoc(..))
import TsBridgeGen.Types (AppError(..))
import TsBridgeGen.UIText as UIText
import Type.Function (type ($))
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import TypedEnv as TypedEnv

type InitM a = ExceptT AppError Effect a

runInitM :: forall a. InitM a -> Effect a
runInitM ma = ma
  # runExceptT
  # try
  >>= handleErrors

--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------

newtype AppConfig = AppConfig
  { | AppConfig_Mandatory + AppConfig_Optional It + () }

type AppConfig_Mandatory r =
  ( assetsDir :: String
  | r
  )

type AppConfig_Optional :: (Type -> Type) -> Row Type -> Row Type
type AppConfig_Optional maybe r =
  ( modulesFile :: maybe String
  , classFile :: maybe String
  , debug :: maybe Boolean
  | r
  )

defaults :: { | AppConfig_Optional It () }
defaults =
  { modulesFile: "src/MyTsBridgeModules.purs"
  , classFile: "src/MyTsBridgeClass.purs"
  , debug: false
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
  { classFile: cliOption "class-file" Nothing
      UIText.cli.options.classFile
      defaults.modulesFile
  , modulesFile: cliOption "modules-file" Nothing
      UIText.cli.options.modulesFile
      defaults.classFile
  }

wrapString :: String -> Int -> Array String
wrapString str width = go (Str.split (Pattern " ") str) [] []
  where
  go :: Array String -> Array String -> Array String -> Array String
  go words line lines | Just { head: word, tail: tailWords } <- uncons words =
    let
      newLineCandidate = line <> [ word ]
    in
      if Str.length (mkLine newLineCandidate) <= width then
        go tailWords newLineCandidate lines
      else
        go tailWords [ word ] (lines <> [ mkLine line ])
  go _ word lines = lines <> [ mkLine word ]

  mkLine = Str.joinWith " "

getCliArgs :: InitM AppCliArgs
getCliArgs = liftEffect $ Opt.execParser opts
  where
  opts = Opt.info (optParser <**> Opt.helper)
    ( fold
        [ Opt.fullDesc
        , Opt.header UIText.cli.header
        , Opt.footerDoc $ unwrap $
            vcatChunks
              [ stringChunk "Environment variables:"
              , tabulate
                  [ (extractChunk $ stringChunk "DEBUG") /\ (extractChunk $ stringChunk "BOOLEAN")
                  ]
              ]
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
  , debug :: f "DEBUG" $ Maybe Boolean
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
    , debug: env.debug
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

fromMaybeRecord
  :: forall r1 r2 r3
   . HMapWithIndex (MappingFromMaybeRecord r1) { | r2 } { | r3 }
  => { | r1 }
  -> { | r2 }
  -> { | r3 }
fromMaybeRecord = hmapWithIndex <<< MappingFromMaybeRecord

newtype MappingFromMaybeRecord r = MappingFromMaybeRecord { | r }

instance
  ( IsSymbol sym
  , Row.Cons sym a x r
  ) =>
  MappingWithIndex (MappingFromMaybeRecord r) (Proxy sym) (Maybe a) a where
  mappingWithIndex (MappingFromMaybeRecord rec) prop = fromMaybe (Record.get prop rec)

cliOption :: forall a. ReadM a => String -> Maybe Char -> String -> a -> Opt.Parser (Maybe a)
cliOption long short help def =
  (Opt.option $ optional $ readM) $ fold
    [ Opt.long long
    , maybe mempty Opt.short short
    , Opt.helpDoc
        ( Just $ extractChunk $ vcatChunks
            ( (wrapString help 60 <#> stringChunk)
                <>
                  [ stringChunk $ "Defaults to " <> writeM def
                  , stringChunk " "
                  ]
            )
        )
    , Opt.value Nothing
    , Opt.metavar $ metavar (Proxy :: _ a)
    ]

class ReadM a where
  readM :: Opt.ReadM a
  writeM :: a -> String
  metavar :: Proxy a -> String

instance ReadM String where
  readM = Opt.str
  writeM x = x
  metavar _ = "STRING"

handleErrors :: forall a. Error \/ AppError \/ a -> Effect a
handleErrors = case _ of
  Left err -> do
    debug <- lookupEnv "DEBUG" <#> case _ of
      Just "true" -> true
      _ -> false

    quitWithError
      if debug then
        show err
      else
        UIText.init.unexpectedError

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