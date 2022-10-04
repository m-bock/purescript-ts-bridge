module TsBridgeGen.Cli
  ( app
  , parseStrToData
  , patchClassFile
  , replaceComment
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, catchError, liftEither, throwError, try)
import Control.Monad.Reader (ask, lift)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Argonaut (class DecodeJson, decodeJson)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either, either)
import Data.Maybe (fromMaybe)
import Data.Set as Set
import Data.String (Pattern(..), Replacement(..))
import Data.String as Str
import Data.Traversable (and, for, or)
import Data.Tuple.Nested ((/\))
import Effect.Aff (throwError)
import Node.FS.Aff (writeTextFile)
import Node.Path (FilePath, dirname)
import Node.Path as Path
import Parsing (ParserT, Position(..), fail, position)
import Parsing.Combinators (sepBy)
import Parsing.String (anyTill, string) as P
import Parsing.String.Replace (replaceT) as P
import Safe.Coerce (coerce)
import TsBridgeGen (class MonadLog, AppCapabalities(..), AppError(..), Import(..), ModuleName(..), Name(..), PursModule(..), ReplaceImportsOpts, ReplaceInstancesOpts, SourcePosition(..), genInstances, genTsProgram, getName, getPursModule, parseCstModule, parseToJson, parseUserImports, printImports, printPursSnippets, runImportWriterT)
import TsBridgeGen.Config (AppConfig(..))
import TsBridgeGen.Core (ReplaceTsProgramOpts)
import TsBridgeGen.Monad (class MonadApp, AppEnv(..), log)
import TsBridgeGen.Types (AppError(..), AppLog(..), Glob(..), ModuleGlob(..), PursModule)

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

assets :: _
assets =
  { myTsBridgeClass: "MyTsBridgeClass.purs"
  , myTsModules: "MyTsBridgeModules.purs"
  , allDeps: "all-deps.dhall"
  , packagesFile: "packages.dhall"
  , spagoFile: "spago.dhall"
  }

updateFile :: forall m. MonadApp m => m String -> FilePath -> (String -> m String) -> m Unit
updateFile fallback filePath f = do
  AppEnv
    { capabilities: AppCapabalities
        { readTextFile
        , writeTextFile
        , mkdirRec
        }
    } <- ask

  content <- (readTextFile filePath <* (log $ LogLiteral ("Patching module at " <> filePath))) `catchError`
    case _ of
      ErrReadFile _ ->
        fallback
          <* (log $ LogLiteral ("Module at " <> filePath <> " does not exist yet. Using a starter template."))
      e -> throwError e

  content' <- f content
  mkdirRec (dirname filePath)
  writeTextFile filePath content'

writeFileIfNotExists :: forall m. MonadApp m => m String -> FilePath -> m Unit
writeFileIfNotExists fallback filePath = do
  AppEnv
    { capabilities: AppCapabalities
        { readTextFile
        , writeTextFile
        , mkdirRec
        }
    } <- ask

  (void $ readTextFile filePath) `catchError` case _ of
    ErrReadFile _ -> do
      content <- fallback
      mkdirRec (dirname filePath)
      writeTextFile filePath content
    e -> throwError e

matchModuleGlob :: ModuleGlob -> ModuleName -> Name -> Boolean
matchModuleGlob (ModuleGlob mg) (ModuleName mn) (Name n) =
  match (Glob $ replace mg) (replace (mn <> "." <> n))
  where
  replace = Str.replaceAll (Pattern ".") (Replacement "/")

foreign import match :: Glob -> String -> Boolean

patchClassFile
  :: forall m
   . MonadApp m
  => FilePath
  -> Array PursModule
  -> String
  -> m String
patchClassFile path defs file = do
  file' /\ { imports } <- file
    # replaceComment path "instances" replaceInstances
    # runImportWriterT

  file'
    # replaceComment path "imports" (replaceImports imports)
  where
  replaceInstances opts _ = do
    instances <- defs
      <#> mapModule opts
      # genInstances
    pure $ printPursSnippets instances

  replaceImports imports (_ :: ReplaceImportsOpts) oldImports = oldImports
    # parseUserImports
    <#> Set.map ImportUser
    # fromMaybe Set.empty
    # Set.union imports
    # printImports
    # pure

patchModulesFile
  :: forall m
   . MonadApp m
  => FilePath
  -> Array PursModule
  -> String
  -> m String
patchModulesFile path defs file = do
  file' /\ { imports } <- file
    # replaceComment path "ts-program" replaceTsProgram
    # runImportWriterT

  file'
    # replaceComment path "imports" (replaceImports imports)
  where
  replaceTsProgram (opts :: ReplaceTsProgramOpts) _ = do
    tsProgram <- defs
      <#> mapModule opts
      # A.filter (\(PursModule _ defs') -> not $ A.null defs')
      # genTsProgram
    pure $ tsProgram

  replaceImports imports (_ :: ReplaceImportsOpts) oldImports = oldImports
    # parseUserImports
    <#> Set.map ImportUser
    # fromMaybe Set.empty
    # Set.union imports
    # printImports
    # pure

mapModule :: ReplaceInstancesOpts -> PursModule -> PursModule
mapModule opts (PursModule mn defs) =
  defs
    # A.filter (getName >>> filterName)
    # PursModule mn
  where
  matcher glob = matchModuleGlob glob mn
  filterName = (or $ matcher <$> opts.include) &&
    (and $ not matcher <$> opts.exclude)

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
    (liftEither $ parseStrToData json) `catchError`
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

parseStrToData :: forall a. DecodeJson a => String -> Either AppError a
parseStrToData str = str
  # parseToJson
  # lmap ErrParseToJson
  >>= decodeJson >>> lmap ErrParseToData

app :: forall m. MonadApp m => m Unit
app = do
  AppEnv
    { config: AppConfig
        { classFile
        , modulesFile
        , allDepsFile
        , packagesFile
        , spagoFile
        }
    , capabilities: AppCapabalities
        { spagoSources
        }
    } <- ask

  globs <- spagoSources
  defs <- getPursModules globs

  updateFile (readAsset assets.myTsBridgeClass)
    classFile $
    patchClassFile classFile defs

  updateFile (readAsset assets.myTsModules)
    modulesFile $
    patchModulesFile modulesFile defs

  updateFile (readAsset assets.allDeps)
    allDepsFile $
    updateAllDepsFile allDepsFile

  updateFile (readAsset assets.packagesFile)
    packagesFile
    pure

  writeFileIfNotExists (readAsset assets.spagoFile)
    spagoFile

readAsset :: forall m. MonadApp m => String -> m String
readAsset path = do
  AppEnv
    { config: AppConfig { assetsDir }
    , capabilities: AppCapabalities { readTextFile }
    } <- ask

  readTextFile $ Path.concat [ assetsDir, path ]

updateAllDepsFile :: forall m. MonadApp m => FilePath -> String -> m String
updateAllDepsFile _ _ = do
  AppEnv
    { capabilities: AppCapabalities { spagoLsDepsTransitive } } <- ask

  deps <- spagoLsDepsTransitive

  let
    dhallExpr = (deps <#> _.packageName >>> DhallExprString # DhallExprList)
      `DhallExprTypeAnnot` (DhallTypeId "List" `DhallTypeApp` DhallTypeId "Text")

  pure $ printDhallExpr dhallExpr

data DhallExpr
  = DhallExprList (Array DhallExpr)
  | DhallExprString String
  | DhallExprTypeAnnot DhallExpr DhallType

data DhallType
  = DhallTypeApp DhallType DhallType
  | DhallTypeId String

printDhallExpr :: DhallExpr -> String
printDhallExpr = case _ of
  DhallExprList xs -> "[\n" <> (Str.joinWith ",\n" $ printDhallExpr <$> xs) <> "\n]"
  DhallExprString s -> "\"" <> s <> "\""
  DhallExprTypeAnnot e t -> printDhallExpr e <> " : " <> printDhallType t

printDhallType :: DhallType -> String
printDhallType = case _ of
  DhallTypeApp t1 t2 -> printDhallType t1 <> " " <> printDhallType t2
  DhallTypeId s -> s


-- parserDhallExpr :: forall m. Monad m => ParserT String m DhallExpr
-- parserDhallExpr =
--   (do
--       _ <- P.string "["
--       xs <- parserDhallExpr `sepBy` P.string ","
--       _ <- P.string "]"
--       pure $ DhallExprList $ A.fromFoldable xs
--   )