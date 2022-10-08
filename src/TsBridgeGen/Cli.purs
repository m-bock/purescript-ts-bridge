module TsBridgeGen.Cli
  ( app
  , parseStrToData
  , patchClassFile
  , patchModulesFile
  , replaceComment
  ) where

import Prelude

import Control.Monad.Error.Class (catchError, liftEither, try)
import Control.Monad.Reader (lift)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Argonaut (class DecodeJson, decodeJson)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either, either, hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.String (Pattern(..), Replacement(..))
import Data.String as Str
import Data.String.Regex as Regex
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (and, for, or)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Undefined (undefined)
import Dodo (twoSpaces)
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (throwError)
import Effect.Unsafe (unsafePerformEffect)
import Node.Path (FilePath, dirname)
import Node.Path as Path
import Parsing (fail, position)
import Parsing.String (anyTill, string) as P
import Parsing.String.Replace (replaceT) as P
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Types (SourcePos)
import Safe.Coerce (coerce)
import Tidy (defaultFormatOptions, formatModule)
import Tidy.Doc (FormatDoc(..))
import TsBridgeGen (AppEffects(..), ErrorParseToJson(..), FileSection(..), Import(..), ModuleName(..), Name(..), PursModule(..), ReplaceImportsOpts, ReplaceInstancesOpts, SourcePosition(..), genInstances, genTsProgram, getName, getPursModule, indexToSourcePos, parseCstModule, parseToJson, parseUserImports, positionToSourcePosition, printImports, printPursSnippets, pushError, runImportWriterT)
import TsBridgeGen.Config (AppConfig(..))
import TsBridgeGen.Core (ReplaceTsProgramOpts)
import TsBridgeGen.Monad (class MonadApp, askAppConfig, askAppEffects, log)
import TsBridgeGen.Types (AppError(..), AppLog(..), Glob(..), ModuleGlob(..), PursModule)

-------------------------------------------------------------------------------
-- App
-------------------------------------------------------------------------------

foreign import runPrettierImpl :: String -> Effect String

runPrettier :: String -> Maybe String
runPrettier str = str
  # runPrettierImpl
  # try
  # unsafePerformEffect
  # hush

getPursModules :: forall m. MonadApp m => Array Glob -> m (Array PursModule)
getPursModules globs = do
  AppEffects { readTextFile } <- askAppEffects

  paths <- getPaths globs
  sources <- for paths readTextFile
  cstModules <- for sources (liftEither <<< parseCstModule)
  pure $ getPursModule <$> cstModules

getPaths :: forall m. MonadApp m => Array Glob -> m (Array String)
getPaths globs = do
  AppEffects { expandGlobsCwd } <- askAppEffects

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
  AppEffects { readTextFile, writeTextFile, mkdirRec } <- askAppEffects

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
  AppEffects { readTextFile, writeTextFile, mkdirRec } <- askAppEffects

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
    # replaceComment path (FileSection "instances") replaceInstances
    # runImportWriterT

  file'
    # replaceComment path (FileSection "imports") (replaceImports imports)
    <#> (\str -> tidyPurs str # fromMaybe str)
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

tidyPurs :: String -> Maybe String
tidyPurs str = str # parseModule >>> case _ of
  ParseSucceeded m ->
    formatModule defaultFormatOptions m
      # (\(FormatDoc { doc }) -> doc)
      # Dodo.print Dodo.plainText
          twoSpaces
      # Just
  _ -> Nothing

patchModulesFile
  :: forall m
   . MonadApp m
  => FilePath
  -> Array PursModule
  -> String
  -> m String
patchModulesFile path defs file = do
  file' /\ { imports } <- file
    # replaceComment path (FileSection "ts-program") replaceTsProgram
    # runImportWriterT

  file'
    # replaceComment path (FileSection "imports") (replaceImports imports)
    <#> (\str -> tidyPurs str # fromMaybe str)
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

type GenComment =
  { id :: String
  , options :: String
  , content :: String
  }

replaceComment
  :: forall m a
   . MonadRec m
  => MonadApp m
  => DecodeJson a
  => FilePath
  -> FileSection
  -> (a -> String -> m String)
  -> String
  -> m String
replaceComment path fs@(FileSection id) f i = P.replaceT i do
  genStartOpen <- P.string ("{-GEN:" <> id <> "\n")

  posJson <- position <#> positionToSourcePosition
  jsonStr /\ genStartClose <- P.anyTill (P.string "\n-}\n")
    <#> lmap (\j -> runPrettier j # fromMaybe j)

  posContent <- position <#> positionToSourcePosition
  content /\ genEnd <- P.anyTill (P.string "\n{-GEN:END-}")

  let
    getJson = (parseToJson jsonStr # lmap ErrParseToJson # liftEither) `catchError`
      \e -> do
        let
          p = fromMaybe mempty case e of
            ErrParseToJson (UnexpectedTokenAtPos _ idx) -> indexToSourcePos jsonStr idx
            ErrParseToJson (UnexpectedEndOfInput) -> indexToSourcePos jsonStr (Str.length jsonStr)
            _ -> Nothing

        pushError $ AtFileSection
          { path
          , section: fs
          , filePos: Nothing
          , localPos: mempty
          }
          e
        throwError e

  let
    getData json = (json # decodeJson # lmap ErrParseToData # liftEither) `catchError`
      \e -> do
        pushError $ AtFileSection
          { path
          , section: fs
          , filePos: Nothing
          , localPos: mempty
          }
          e
        throwError e

  let
    getContent data_ = f data_ content `catchError`
      \appError -> do
        pushError $ AtFileSection
          { path
          , section: fs
          , filePos: Nothing
          , localPos: mempty
          }
          appError
        throwError appError

  let
    finalize json newContent =
      genStartOpen
        <> Str.trim json
        <> genStartClose
        <> "\n"
        <> newContent
        <> "\n"
        <> genEnd

  let
    getReplacement =
      do
        json <- getJson
        do
          data_ <- getData json
          newContent <- getContent data_
          pure $ finalize jsonStr newContent
        `catchError`
          \e -> pure $ finalize jsonStr content

  getReplacement
    # try
    # lift
    >>= either (const $ fail "Cannot parse Json") pure

parseStrToData :: forall a. DecodeJson a => String -> Either AppError a
parseStrToData str = str
  # parseToJson
  # lmap ErrParseToJson
  >>= decodeJson >>> lmap ErrParseToData

app :: forall m. MonadApp m => m Unit
app = do
  AppConfig
    { classFile
    , modulesFile
    , allDepsFile
    , packagesFile
    , spagoFile
    } <- askAppConfig

  AppEffects { spagoSources } <- askAppEffects

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

  pure unit

readAsset :: forall m. MonadApp m => String -> m String
readAsset path = do
  AppConfig { assetsDir } <- askAppConfig
  AppEffects { readTextFile } <- askAppEffects

  readTextFile $ Path.concat [ assetsDir, path ]

updateAllDepsFile :: forall m. MonadApp m => FilePath -> String -> m String
updateAllDepsFile _ _ = do
  AppEffects { spagoLsDepsTransitive } <- askAppEffects

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

sourcePosOfSection :: String -> FileSection -> Maybe SourcePosition
sourcePosOfSection txt fs =
  Regex.search regex txt
    >>= indexToSourcePos txt
  where
  regex = unsafeRegex ("{-GEN:" <> unwrap fs <> "\n") global
