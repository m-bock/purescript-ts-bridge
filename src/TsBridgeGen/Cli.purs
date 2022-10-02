module TsBridgeGen.Cli
  ( app
  , patchClassFile
  , replaceComment
  )
  where

import Prelude

import Control.Monad.Error.Class (class MonadError, catchError, liftEither, try)
import Control.Monad.Reader (ask, lift)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Argonaut (class DecodeJson, decodeJson)
import Data.Bifunctor (lmap)
import Data.Either (either)
import Data.Maybe (fromMaybe)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as Str
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (throwError)
import Node.Path (FilePath)
import Node.Path as Path
import Parsing (Position(..), fail, position)
import Parsing (fail) as P
import Parsing.String (anyTill, eof, string) as P
import Parsing.String.Basic (intDecimal) as P
import Parsing.String.Replace (replaceT) as P
import Safe.Coerce (coerce)
import TsBridgeGen (class MonadLog, AppCapabalities(..), Import(..), ReplaceImportsOpts, ReplaceInstancesOpts, SourcePosition(..), genInstances, getPursModule, parseCstModule, parseToJson, parseUserImports, printImports, printPursSnippets, runImportWriterT)
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

patchClassFile
  :: forall m
   . MonadApp m
  => FilePath
  -> Array PursModule
  -> String
  -> m String
patchClassFile path defs file = file
  # replaceComment path "instances"
      ( \(_ :: ReplaceInstancesOpts) _ -> do
          instances <- defs # genInstances
          pure $ printPursSnippets instances
      )
  # runImportWriterT
  >>=
    ( \(file' /\ { imports }) -> file'
        # replaceComment path "imports"
            ( \(_ :: ReplaceImportsOpts) oldImports -> do

                oldImports
                  # parseUserImports
                  <#> Set.map ImportUser
                  # fromMaybe Set.empty
                  # Set.union imports
                  # printImports
                  # pure
            )

    )

replaceComment
  :: forall m a
   . MonadRec m
  => MonadError AppError m
  => MonadLog AppLog m
  => DecodeJson a
  --  => MonadEffect m
  => FilePath
  -> String
  -> (a -> String -> m String)
  -> String
  -> m String
replaceComment path id f i = P.replaceT i do
  Position pos <- position
  genStartOpen <- P.string ("{-GEN:" <> id <> "\n")
  json /\ genStartClose <- P.anyTill (P.string "\n-}\n")
  content /\ genEnd <- P.anyTill (P.string "\n{-GEN:END-}")

  let sourcePos = SourcePosition { line: pos.line, column: pos.column }

  data_ <-
    (liftEither $ decode json) `catchError`
      ( \appError -> do
          log $ LogError $ AtFilePosition path sourcePos appError
          throwError appError
      )
      # try
      # lift
      >>= either (const $ fail "Cannot parse Json") pure

  newContent <-
    f data_ content `catchError`
      ( \appError -> do
          log $ LogError $ AtFilePosition path sourcePos appError
          throwError appError
      )
      # try
      # lift
      >>= either (const $ fail "Execution failure") pure

  --  newJson <- lift $ liftEffect $ runPrettier json

  pure (genStartOpen <> json <> genStartClose <> "\n" <> newContent <> "\n" <> genEnd)
  where
  decode str = str
    # parseToJson
    # lmap ErrParseToJson
    >>= decodeJson >>> lmap ErrParseToData
app :: forall m. MonadApp m => m Unit
app = do
  AppEnv { config: AppConfig { classFile } } <- ask

  globs <- getSpagoGlobs
  defs <- getPursModules globs

  updateFile classFile $ patchClassFile classFile defs