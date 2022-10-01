module TsBridgeGen.Cli (main) where

import Prelude

import Control.Monad.Error.Class (catchError, liftEither, throwError)
import Control.Monad.Reader (ask, asks)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as Str
import Data.Traversable (for)
import Effect (Effect)
import Effect.Aff (Aff, Error, throwError, try)
import Effect.Aff.Class (liftAff)
import Effect.Exception (throw)
import Node.ChildProcess (Exit(..), defaultSpawnOptions)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Node.FS.Aff as FS
import Node.Glob.Basic as Glob
import Node.Path (FilePath)
import Node.Path as Path
import Safe.Coerce (coerce)
import Sunde as Sun
import TsBridgeGen (AppError(..), getPursModule, parseCstModule, patchClassFile)
import TsBridgeGen.Config (AppConfig(..), getConfig, runInitM)
import TsBridgeGen.Monad (AppEnv(..), AppM, log, runAppM)
import TsBridgeGen.Types (AppError(..), AppLog(..), Glob(..), PursModule)

-------------------------------------------------------------------------------
-- App
-------------------------------------------------------------------------------

getPursModules :: Array Glob -> AppM (Array PursModule)
getPursModules globs = do
  paths <- getPaths globs
  sources <- for paths readTextFile
  cstModules <- for sources (liftEither <<< parseCstModule)
  pure $ getPursModule <$> cstModules

getPaths :: Array Glob -> AppM (Array String)
getPaths globs = Glob.expandGlobsCwd (coerce globs)
  # liftAffWithErr (const ErrExpandGlobs)
  <#> Set.toUnfoldable

readTextFile :: String -> AppM String
readTextFile path = FS.readTextFile UTF8 path
  # liftAffWithErr (const $ ErrReadFile path)

assets :: _
assets =
  { myTsBridgeClass: "MyTsBridgeClass.purs"
  }

updateFile :: FilePath -> (String -> AppM String) -> AppM Unit
updateFile filePath f = do
  AppEnv { config: AppConfig { assetsDir } } <- ask

  content <- (readTextFile filePath <* (log $ LogLiteral ("Patching module at " <> filePath))) `catchError`
    case _ of
      ErrReadFile _ ->
        readTextFile (Path.concat [ assetsDir, assets.myTsBridgeClass ])
          <* (log $ LogLiteral ("Module at " <> filePath <> " does not exist yet. Using a starter template."))
      e -> throwError e

  content' <- f content
  liftAff $ writeTextFile UTF8 filePath content'

liftAffWithErr :: forall a. (Error -> AppError) -> Aff a -> AppM a
liftAffWithErr mkError ma = ma
  # try
  <#> lmap mkError
  # liftAff
  >>= liftEither

getSpagoGlobs :: AppM (Array Glob)
getSpagoGlobs = spawn "spago" [ "sources" ]
  <#> _.stdout
    >>> Str.split (Pattern "\n")
    >>> map Glob

spawn :: String -> Array String -> AppM { stderr :: String, stdout :: String }
spawn cmd args = do
  { exit, stderr, stdout } <-
    liftAff $ Sun.spawn { cmd, args, stdin: Nothing }
      defaultSpawnOptions
  case exit of
    Normally 0 -> pure { stderr, stdout }
    _ -> throwError $ ErrSpawn cmd args

app :: AppM Unit
app = do
  AppEnv { config: AppConfig { classFile } } <- ask

  globs <- getSpagoGlobs
  defs <- getPursModules globs

  updateFile classFile $ patchClassFile classFile defs

main :: Effect Unit
main = do
  config <- runInitM getConfig
  let appEnv = AppEnv { config }
  runAppM appEnv app
