-- | This module contains code that can turn a TsProgram into a CLI that writes
-- | it to the file system.
-- |
module TsBridge.Cli (mkTypeGenCli) where

import Prelude

import Data.Foldable (fold, for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Node.ChildProcess (Exit(..), defaultSpawnOptions)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (mkdir', writeTextFile)
import Node.FS.Perms (all, mkPerms)
import Node.Path (dirname)
import Options.Applicative (help, helper, info, long, metavar, strOption, value, (<**>))
import Options.Applicative as O
import Options.Applicative.Types (optional)
import Sunde as Sun
import TsBridge.DTS (TsProgram)
import TsBridge.Print (Path(..), TsSource(..), printTsProgram)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
type TsBridgeCliOpts =
  { outputDir :: Path
  , prettier :: Maybe String
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
  prettier <- optional
    $ strOption
    $ fold
        [ long "prettier"
        , metavar "PRETTIER"
        , help "Path to prettier to format generated code. If omitted, code is not formatted."
        ]
  in { outputDir, prettier }

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
          filePath = cliOpts.outputDir <> Path "/" <> modPath
        log $ un Path filePath
        mkdir' (dirname $ un Path filePath)
          { recursive: true
          , mode: mkPerms all all all
          }
        writeTextFile UTF8 (un Path filePath) (un TsSource source)
    )

  case cliOpts.prettier of
    Just prettierPath ->
      void $ spawn prettierPath
        [ "--write", un Path (cliOpts.outputDir <> Path "/**/*.d.ts") ] -- can fail, if there are no files!
    Nothing ->
      log "No path to prettier specified. Generated files will be ugly."

-- | Given a `TsProgram` returns an effectful CLI that can be used as an entry
-- | point for a type generator.
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
