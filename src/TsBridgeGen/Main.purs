module TsBridgeGen.Main where

import Prelude

import Control.Monad.Error.Class (liftEither)
import Data.Argonaut (class DecodeJson, printJsonDecodeError)
import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.String (Pattern(..))
import Data.String as Str
import Data.Traversable (for, for_, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Undefined (undefined)
import Dodo (Doc)
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, Error, launchAff_, throwError, try)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Class.Console as E
import Node.ChildProcess (Exit(..), defaultSpawnOptions)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.FS.Perms as FS
import Node.Glob.Basic as Glob
import Node.Path (FilePath)
import Node.Process as Process
import PureScript.CST (RecoveredParserResult(..))
import PureScript.CST as CST
import Sunde as Sun
import Tidy as Tidy
import Tidy.Doc (FormatDoc(..))
import TsBridgeGen (AppEffects(..), AppError(..), AppLog(..), Glob(..))
import TsBridgeGen.Cli (app, parseStrToData)
import TsBridgeGen.Config (AppConfig(..), getConfig, runInitM)
import TsBridgeGen.Monad (AppEnv(..), AppM, MonadAppAccum, runAppM)
import TsBridgeGen.Types (AppError(..))

main :: Effect Unit
main = do
  config <- runInitM getConfig
  let
    appEnv = AppEnv
      { config
      , capabilities: AppEffects
          { writeTextFile
          , readTextFile
          , expandGlobsCwd
          , runPrettier
          , mkdirRec
          , spagoLsDepsTransitive
          , spagoSources
          }
      }

  launchAff_ do
    Tuple res accum <- runAppM appEnv app
    handleAccum config accum
    handleErrors config (Array.length accum.errors) res

handleAccum :: AppConfig -> MonadAppAccum -> Aff Unit
handleAccum config { logs, errors } = do
  let
    l = logs
      <#> (printAppLog config)
      # Dodo.lines

  let
    e = (errors <> errors <> errors)
      # mapWithIndex
          ( \idx error -> error
              # printOneOfManyErrors config idx (Array.length errors)
          )
      # Dodo.foldWithSeparator (Dodo.words [ Dodo.break, Dodo.break ])

  Dodo.foldWithSeparator (Dodo.words [ Dodo.break, Dodo.break ]) [ l, e ]
    # printDoc
    # Console.error

printOneOfManyErrors :: forall a. AppConfig -> Int -> Int -> AppError -> Doc a
printOneOfManyErrors config idx count e =
  Dodo.lines
    [ Dodo.text ("Error " <> show (idx + 1) <> " of " <> show count <> ":")
    , Dodo.indent $ printAppError config e
    ]

printAppLog :: forall a. AppConfig -> AppLog -> Doc a
printAppLog config@(AppConfig { debug }) x =
  if debug then showDoc x
  else case x of
    LogLiteral str -> str # Str.split (Pattern "\n") <#> Dodo.text # Dodo.lines

handleErrors :: forall a. AppConfig -> Int -> Error \/ AppError \/ a -> Aff a
handleErrors config@(AppConfig { debug }) errorCount = case _ of
  Left err ->
    if debug then
      quitWithError ("Unexpected Error.\n" <> show err)
    else
      quitWithError "Unexpected Error. Try to set DEBUG=true"
  Right (Left appError) ->
    quitWithError $ printDoc $ printAppError config appError
  Right (Right _) | errorCount /= 0 ->
    quitWithError ""
  Right (Right x) -> pure x

printDoc = Dodo.print Dodo.plainText Dodo.twoSpaces

quitWithError :: forall a. String -> Aff a
quitWithError msg = do
  Console.error msg
  liftEffect $ Process.exit 1

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

printAppError :: forall a. AppConfig -> AppError -> Doc a
printAppError config@(AppConfig { debug }) x =
  if debug then showDoc x
  else case x of
    ErrSpawn cmd _ ->
      Dodo.text ("Failed to spawn Command " <> cmd)
    ErrParseModule ->
      Dodo.text ("Failed to parse PureScript module")
    ErrReadFile path ->
      Dodo.text ("Failed to read from file " <> path)
    ErrWriteFile path ->
      Dodo.text ("Failed to write to file " <> path)
    ErrExpandGlobs ->
      Dodo.text "Failed to expand globs"
    ErrParseEnvVars _ ->
      Dodo.text "Failed to parse environment variables"
    ErrLiteral str -> str
      # Str.split (Pattern "\n")
      <#> Dodo.text
      # Dodo.lines
    ErrParseToJson _ ->
      Dodo.text "Found invalid JSON"
    ErrParseToData je ->
      Dodo.lines $ map Dodo.text $ Str.split (Pattern "\n") $ printJsonDecodeError je
    ErrUnknown ->
      Dodo.text "An unknown error occured. Try DEBUG=true" --
    AtFileSection fp s e -> Dodo.lines
      [ printAppError config e
      , Dodo.text ("in file " <> fp <> " at section: " <> s)
      ]

showDoc :: forall a b. Show a => a -> Doc b
showDoc = show >>> CST.parseExpr >>> case _ of
  ParseSucceeded expr -> Tidy.formatExpr Tidy.defaultFormatOptions expr
    # (\(FormatDoc { doc }) -> doc)
  _ -> Dodo.text "<invalid>"
