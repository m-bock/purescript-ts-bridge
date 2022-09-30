module TsBridgeGen.Cli (main) where

import Prelude

import Control.Monad.Error.Class (liftEither)
import Data.Bifunctor (lmap)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as Str
import Data.Traversable (for)
import Data.Typelevel.Undefined (undefined)
import Effect (Effect)
import Effect.Aff (Aff, Error, throwError, try)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Node.ChildProcess (Exit(..), defaultSpawnOptions)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Node.FS.Aff as FS
import Node.Glob.Basic as Glob
import Node.Path (FilePath)
import Options.Applicative (ReadM, help, helper, info, long, metavar, strOption, value, (<**>))
import Options.Applicative as O
import Options.Applicative as Opt
import Options.Applicative.Types (ReadM(..), optional)
import Record.Extra (sequenceRecord)
import Safe.Coerce (coerce)
import Sunde as Sun
import TsBridgeGen (getPursModule, parseCstModule, patchClassFile)
import TsBridgeGen.Config (getConfig, runInitM)
import TsBridgeGen.Monad (TsBridgeGenM, runTsBridgeGenM)
import TsBridgeGen.Types (TsBridgeGenError(..), Glob(..), PursModule)

-------------------------------------------------------------------------------
-- App
-------------------------------------------------------------------------------

getPursModules :: Array Glob -> TsBridgeGenM (Array PursModule)
getPursModules globs = do
  paths <- getPaths globs
  sources <- for paths readTextFile
  cstModules <- for sources (liftEither <<< parseCstModule)
  pure $ getPursModule <$> cstModules

getPaths :: Array Glob -> TsBridgeGenM (Array String)
getPaths globs = Glob.expandGlobsCwd (coerce globs)
  # liftAffWithErr (const ErrExpandGlobs)
  <#> Set.toUnfoldable

readTextFile :: String -> TsBridgeGenM String
readTextFile path = FS.readTextFile UTF8 path
  # liftAffWithErr (const $ ErrReadFile path)

updateFile :: FilePath -> (String -> String) -> TsBridgeGenM Unit
updateFile filePath f = do
  content <- readTextFile filePath
  let content' = f content
  liftAff $ writeTextFile UTF8 filePath content'
  log ("Patched GEN-comments in " <> filePath)

liftAffWithErr :: forall a. (Error -> TsBridgeGenError) -> Aff a -> TsBridgeGenM a
liftAffWithErr mkError ma = ma
  # try
  <#> lmap mkError
  # liftAff
  >>= liftEither

getSpagoGlobs :: TsBridgeGenM (Array Glob)
getSpagoGlobs = spawn "spago" [ "sources" ]
  <#> _.stdout
    >>> Str.split (Pattern "\n")
    >>> map Glob

spawn :: String -> Array String -> TsBridgeGenM { stderr :: String, stdout :: String }
spawn cmd args = do
  { exit, stderr, stdout } <-
    liftAff $ Sun.spawn { cmd, args, stdin: Nothing }
      defaultSpawnOptions
  case exit of
    Normally 0 -> pure { stderr, stdout }
    _ -> throwError $ ErrSpawn cmd args

app :: TsBridgeGenM Unit
app = do
  globs <- getSpagoGlobs
  defs <- getPursModules globs

  updateFile "src-ts-gen/SampleTsGen/TsBridgeClass.purs" $ patchClassFile defs

main :: Effect Unit
main = do
  appConfig <- runInitM getConfig
  runTsBridgeGenM appConfig app
