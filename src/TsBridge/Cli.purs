-- | This module contains code that can turn a TsProgram into a CLI that writes
-- | it to the file system.
-- |
module TsBridge.Cli (mkTypeGenCli) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (fold, for_)
import Data.Map as Map
import Data.Newtype (un)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (mkdir', writeTextFile)
import Node.FS.Perms (all, mkPerms)
import Node.Path (dirname)
import Node.Process as Process
import Options.Applicative (help, helper, info, long, metavar, strOption, value, (<**>))
import Options.Applicative as O
import TsBridge.DTS (TsProgram, Error, printError)
import TsBridge.Print (Path(..), TsSource(..), printTsProgram)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
type TsBridgeCliOpts =
  { outputDir :: Path
  }

-------------------------------------------------------------------------------
-- CLI Options
-------------------------------------------------------------------------------
parserTsBridgeCliOpts :: O.Parser TsBridgeCliOpts
parserTsBridgeCliOpts = ado
  outputDir <-
    Path <$>
      ( strOption
          $ fold
              [ long "output-dir"
              , metavar "OUTPUT_DIR"
              , help "Dictionary the CLI will write the output d.ts files to."
              , value $ "output"
              ]
      )
  in { outputDir }

parserInfoTsBridgeCliOpts :: O.ParserInfo TsBridgeCliOpts
parserInfoTsBridgeCliOpts = info (parserTsBridgeCliOpts <**> helper)
  ( O.fullDesc
      <> O.progDesc "Print a greeting for TARGET"
      <> O.header "hello - a test for purescript-optparse"
  )

-------------------------------------------------------------------------------
-- App
-------------------------------------------------------------------------------
mkTypeGenCliAff :: Either Error TsProgram -> Aff Unit
mkTypeGenCliAff eitherTsProg = do
  cliOpts <- liftEffect $ O.execParser parserInfoTsBridgeCliOpts

  case eitherTsProg of
    Left err -> do
      log $ printError err
      liftEffect $ Process.exit 0
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
mkTypeGenCli :: Either Error TsProgram -> Effect Unit
mkTypeGenCli tsProg = launchAff_ $ mkTypeGenCliAff tsProg

