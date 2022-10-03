module TsBridgeGen.Main where

import Prelude

import Control.Monad.Error.Class (liftEither)
import Data.Argonaut (class DecodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.String (Pattern(..))
import Data.String as Str
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, Error, throwError, try)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Node.ChildProcess (Exit(..), defaultSpawnOptions)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.FS.Perms as FS
import Node.Glob.Basic as Glob
import Node.Path (FilePath)
import Sunde as Sun
import TsBridgeGen (AppCapabalities(..), AppError(..), Glob(..))
import TsBridgeGen.Cli (app, parseStrToData)
import TsBridgeGen.Config (getConfig, runInitM)
import TsBridgeGen.Monad (AppEnv(..), AppM, runAppM)
import TsBridgeGen.Types (AppError(..))

main :: Effect Unit
main = do
  config <- runInitM getConfig
  let
    appEnv = AppEnv
      { config
      , capabilities: AppCapabalities
          { writeTextFile
          , readTextFile
          , expandGlobsCwd
          , runPrettier
          , mkdirRec
          , spagoLsDepsTransitive
          , spagoSources
          }
      }
  runAppM appEnv app

spagoSources :: AppM (Array Glob)
spagoSources = do
  spawn "spago" [ "sources" ]
    <#> _.stdout
      >>> Str.split (Pattern "\n")
      >>> map Glob

expandGlobsCwd :: Array String -> AppM (Set FilePath)
expandGlobsCwd globs = Glob.expandGlobsCwd globs
  # liftAffWithErr (const $ ErrExpandGlobs)

writeTextFile :: FilePath -> String -> AppM Unit
writeTextFile path content = FS.writeTextFile UTF8 path content
  # liftAffWithErr (const $ ErrWriteFile path)

readTextFile :: FilePath -> AppM String
readTextFile path = FS.readTextFile UTF8 path
  # liftAffWithErr (const $ ErrReadFile path)

mkdirRec :: FilePath -> AppM Unit
mkdirRec path =
  FS.mkdir' path
    { recursive: true
    , mode: FS.mkPerms FS.all FS.all FS.all
    }
    # liftAffWithErr (const $ ErrLiteral "make dir")

spagoLsDepsTransitive :: AppM (Array { packageName :: String })
spagoLsDepsTransitive = spawn "spago" [ "ls", "deps", "--transitive", "--json" ]
  >>= _.stdout >>> parseLinesStrToData >>> liftEither

foreign import runPrettierImpl :: String -> Effect String

runPrettier :: String -> AppM String
runPrettier str = runPrettierImpl str
  # liftEffect
  # liftAffWithErr (const $ ErrLiteral "Prettier error")

spawn :: String -> Array String -> AppM { stderr :: String, stdout :: String }
spawn cmd args = do
  { exit, stderr, stdout } <-
    liftAff $ Sun.spawn { cmd, args, stdin: Nothing }
      defaultSpawnOptions
  case exit of
    Normally 0 -> pure { stderr, stdout }
    _ -> throwError $ ErrSpawn cmd args

liftAffWithErr :: forall a. (Error -> AppError) -> Aff a -> AppM a
liftAffWithErr mkError ma = ma
  # try
  <#> lmap mkError
  # liftAff
  >>= liftEither

parseLinesStrToData :: forall a. DecodeJson a => String -> Either AppError (Array a)
parseLinesStrToData str = str
  # Str.trim
  # Str.split (Pattern "\n")
  # traverse parseStrToData