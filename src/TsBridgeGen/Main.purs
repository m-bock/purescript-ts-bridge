module TsBridgeGen.Main where

import Prelude

import Control.Monad.Error.Class (liftEither)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Effect (Effect)
import Effect.Aff (Aff, Error, throwError, try)
import Effect.Aff.Class (liftAff)
import Node.ChildProcess (Exit(..), defaultSpawnOptions)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Glob.Basic as Glob
import Node.Path (FilePath)
import Sunde as Sun
import TsBridgeGen (AppCapabalities(..))
import TsBridgeGen.Cli (app)
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
          { spawn
          , writeTextFile
          , readTextFile
          , expandGlobsCwd
          }
      }
  runAppM appEnv app

expandGlobsCwd :: Array String -> AppM (Set FilePath)
expandGlobsCwd globs = Glob.expandGlobsCwd globs
  # liftAffWithErr (const $ ErrExpandGlobs)

writeTextFile :: FilePath -> String -> AppM Unit
writeTextFile path content = FS.writeTextFile UTF8 path content
  # liftAffWithErr (const $ ErrWriteFile path)

readTextFile :: FilePath -> AppM String
readTextFile path = FS.readTextFile UTF8 path
  # liftAffWithErr (const $ ErrReadFile path)

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
