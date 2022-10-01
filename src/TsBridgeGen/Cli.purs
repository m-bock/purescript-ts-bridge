module TsBridgeGen.Cli (app) where

import Prelude

import Control.Monad.Error.Class (catchError, liftEither)
import Control.Monad.Reader (ask)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as Str
import Data.Traversable (for)
import Effect.Aff (throwError)
import Node.Path (FilePath)
import Node.Path as Path
import Safe.Coerce (coerce)
import TsBridgeGen (AppCapabalities(..), getPursModule, parseCstModule, patchClassFile)
import TsBridgeGen.Config (AppConfig(..))
import TsBridgeGen.Monad (class MonadApp, AppEnv(..), log)
import TsBridgeGen.Types (AppError(..), AppLog(..), Glob(..), PursModule)

-------------------------------------------------------------------------------
-- App
-------------------------------------------------------------------------------

getPursModules :: forall m. MonadApp m => Array Glob -> m (Array PursModule)
getPursModules globs = do
  AppEnv
    { capabilities: AppCapabalities { readTextFile }
    } <- ask

  paths <- getPaths globs
  sources <- for paths readTextFile
  cstModules <- for sources (liftEither <<< parseCstModule)
  pure $ getPursModule <$> cstModules

getPaths :: forall m. MonadApp m => Array Glob -> m (Array String)
getPaths globs = do
  AppEnv
    { capabilities: AppCapabalities { expandGlobsCwd }
    } <- ask

  expandGlobsCwd (coerce globs)
    <#> Set.toUnfoldable

assets
  :: { myTsBridgeClass :: String
     }

assets =
  { myTsBridgeClass: "MyTsBridgeClass.purs"
  }

updateFile :: forall m. MonadApp m => FilePath -> (String -> m String) -> m Unit
updateFile filePath f = do
  AppEnv
    { config: AppConfig { assetsDir }
    , capabilities: AppCapabalities { readTextFile, writeTextFile }
    } <- ask

  content <- (readTextFile filePath <* (log $ LogLiteral ("Patching module at " <> filePath))) `catchError`
    case _ of
      ErrReadFile _ ->
        readTextFile (Path.concat [ assetsDir, assets.myTsBridgeClass ])
          <* (log $ LogLiteral ("Module at " <> filePath <> " does not exist yet. Using a starter template."))
      e -> throwError e

  content' <- f content
  writeTextFile filePath content'

getSpagoGlobs :: forall m. MonadApp m => m (Array Glob)
getSpagoGlobs = do
  AppEnv { capabilities: AppCapabalities { spawn } } <- ask
  spawn "spago" [ "sources" ]
    <#> _.stdout
      >>> Str.split (Pattern "\n")
      >>> map Glob

app :: forall m. MonadApp m => m Unit
app = do
  AppEnv { config: AppConfig { classFile } } <- ask

  globs <- getSpagoGlobs
  defs <- getPursModules globs

  updateFile classFile $ patchClassFile classFile defs