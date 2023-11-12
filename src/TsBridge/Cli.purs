-- | This module contains code that can turn a TsProgram into a CLI that writes
-- | it to the file system.
-- |
module TsBridge.Cli (mkTypeGenCli) where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as Arg
import DTS (TsProgram)
import DTS.Print (Path(..), TsSource(..), printTsProgram)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Map as Map
import Data.Newtype (un)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Aff (mkdir', writeTextFile)
import Node.FS.Perms (all, mkPerms)
import Node.Path (dirname)
import Node.Process as Process
import TsBridge.Types (AppError, printError)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
type TsBridgeCliOpts =
  { outputDir :: Path
  }

-------------------------------------------------------------------------------
-- CLI Options
-------------------------------------------------------------------------------

cliParser :: ArgParser TsBridgeCliOpts
cliParser = Arg.fromRecord
  { outputDir: Path <$> Arg.argument [ "--output-dir" ]
      "Dictionary the CLI will write the output d.ts files to."
  }

getArgs :: Effect TsBridgeCliOpts
getArgs = do
  args <- Array.drop 2 <$> liftEffect Process.argv

  let result = Arg.parseArgs "ts-bridge-cli" "Generates TypeScript Types from PureScript code" cliParser args

  case result of
    Left err -> do
      Console.log $ Arg.printArgError err
      case err of
        Arg.ArgError _ Arg.ShowHelp ->
          Process.exit' 0
        Arg.ArgError _ (Arg.ShowInfo _) ->
          Process.exit' 0
        _ ->
          Process.exit' 1
    Right val -> pure val

-------------------------------------------------------------------------------
-- App
-------------------------------------------------------------------------------
mkTypeGenCliAff :: Either AppError TsProgram -> Aff Unit
mkTypeGenCliAff eitherTsProg = do
  cliOpts <- liftEffect getArgs

  case eitherTsProg of
    Left err -> do
      log ""
      log "Cannot generate TypeScript Code. The following error happened:"
      log ""
      log $ show $ printError err
      log ""
      liftEffect $ Process.exit' 1
    Right tsProg -> writeTsProgramToDisk cliOpts tsProg

writeTsProgramToDisk :: TsBridgeCliOpts -> TsProgram -> Aff Unit
writeTsProgramToDisk cliOpts tsProg = do
  let files = Map.toUnfoldable $ printTsProgram tsProg :: Array _

  for_ files
    ( \(modPath /\ source) -> do
        let
          filePath = cliOpts.outputDir <> Path "/" <> modPath
        log $ un Path filePath
        mkdir' (dirname $ un Path filePath)
          { recursive: true
          , mode: mkPerms all all all
          }
        writeTextFile UTF8 (un Path filePath) (un TsSource source)
    )

-- | Given a `TsProgram` returns an effectful CLI that can be used as an entry
-- | point for a type generator.
mkTypeGenCli :: Either AppError TsProgram -> Effect Unit
mkTypeGenCli tsProg = launchAff_ $ mkTypeGenCliAff tsProg

