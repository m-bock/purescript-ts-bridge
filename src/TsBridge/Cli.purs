module TsBridge.Cli (mkTypeGenCli) where

import Prelude

import Data.Foldable (fold, for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Node.ChildProcess (Exit(..), defaultSpawnOptions)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (mkdir', writeTextFile)
import Node.FS.Perms (all, mkPerm, mkPerms)
import Node.Path (FilePath, dirname)
import Options.Applicative (help, helper, info, long, metavar, strOption, (<**>))
import Options.Applicative as O
import Sunde as Sun
import TsBridge.DTS (TsProgram)
import TsBridge.Print (printTsProgram)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
type TsBridgeCliOpts =
  { outputDir :: String
  }

-------------------------------------------------------------------------------
-- CLI Options
-------------------------------------------------------------------------------
parserTsBridgeCliOpts :: O.Parser TsBridgeCliOpts
parserTsBridgeCliOpts = ado
  outputDir <-
    strOption
      $ fold
          [ long "output-dir"
          , metavar "OUTPUT_DIR"
          , help "Dictionary the CLI will write the output d.ts files to."
          ]
  in { outputDir }

parserInfoTsBridgeCliOpts :: O.ParserInfo TsBridgeCliOpts
parserInfoTsBridgeCliOpts = info (parserTsBridgeCliOpts <**> helper) mempty

-------------------------------------------------------------------------------
-- App
-------------------------------------------------------------------------------
mkTypeGenCliAff :: TsProgram -> Aff Unit
mkTypeGenCliAff tsProg = do
  cliOpts <- liftEffect $ O.execParser parserInfoTsBridgeCliOpts

  let files = Map.toUnfoldable $ printTsProgram tsProg :: Array _

  for_ files
    ( \(modPath /\ source) -> do
        let
          filePath = cliOpts.outputDir <> "/" <> modPath
        log filePath
        mkdir' (dirname filePath)
          { recursive: true
          , mode: mkPerms all all all
          }
        writeTextFile UTF8 filePath source
    )
  void $ spawn "prettier" [ "--write", cliOpts.outputDir <> "/**/*.d.ts" ]

mkTypeGenCli :: TsProgram -> Effect Unit
mkTypeGenCli tsProg = launchAff_ $ mkTypeGenCliAff tsProg

-------------------------------------------------------------------------------
-- Util
-------------------------------------------------------------------------------
spawn :: String -> Array String -> Aff { stderr :: String, stdout :: String }
spawn cmd args = do
  { exit, stderr, stdout } <-
    Sun.spawn { cmd, args, stdin: Nothing }
      defaultSpawnOptions
  case exit of
    Normally 0 -> pure { stderr, stdout }
    _ -> throwError $ error ("Command " <> cmd <> " failed")
