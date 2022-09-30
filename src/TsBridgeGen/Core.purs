module TsBridgeGen.Core where

import Prelude

import Control.Monad.Error.Class (liftEither, throwError, try)
import Control.Monad.Writer (WriterT)
import Data.Array (catMaybes, elem, (:))
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as Str
import Data.String.Regex (replace, replace') as R
import Data.String.Regex.Flags (noFlags) as R
import Data.String.Regex.Unsafe (unsafeRegex) as R
import Data.Traversable (for)
import Data.Tuple (fst, snd)
import Data.Typelevel.Undefined (undefined)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Exception (Error)
import Node.ChildProcess (Exit(..), defaultSpawnOptions)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Glob.Basic as Glob
import Partial.Unsafe (unsafePartial)
import PureScript.CST (RecoveredParserResult(..), parseModule) as CST
import PureScript.CST.Types (Proper(..))
import PureScript.CST.Types as CST
import Safe.Coerce (coerce)
import Sunde as Sun
import TsBridgeGen.Monad (TsBridgeGenError(..), TsBridgeGenM)
import TsBridgeGen.Types (Glob(..), ModuleName(..), Name(..), PursDef(..), PursModule(..))

spawn :: String -> Array String -> TsBridgeGenM { stderr :: String, stdout :: String }
spawn cmd args = do
  { exit, stderr, stdout } <-
    liftAff $ Sun.spawn { cmd, args, stdin: Nothing }
      defaultSpawnOptions
  case exit of
    Normally 0 -> pure { stderr, stdout }
    _ -> throwError $ ErrSpawn cmd args

getSpagoGlobs :: TsBridgeGenM (Array Glob)
getSpagoGlobs = spawn "spago" [ "sources" ]
  <#> _.stdout
    >>> Str.split (Pattern "\n")
    >>> map Glob

getPaths :: Array Glob -> TsBridgeGenM (Array String)
getPaths globs = Glob.expandGlobsCwd (coerce globs)
  # liftAffWithErr (const ErrExpandGlobs)
  <#> (Set.toUnfoldable :: _ -> Array _)

parseCstModule :: String -> Either TsBridgeGenError (CST.Module Void)
parseCstModule mod = case CST.parseModule mod of
  CST.ParseSucceeded m -> pure m
  _ -> throwError ErrParseModule

getPursModule :: CST.Module Void -> PursModule
getPursModule module_ = PursModule
  (ModuleName modName)
  (A.filter (isExported header) $ catMaybes $ getPursDef <$> decls)
  where
  CST.Module { header, body } = module_
  CST.ModuleHeader { name: CST.Name { name: CST.ModuleName modName } } = header
  CST.ModuleBody { decls } = body

isExported :: CST.ModuleHeader Void -> PursDef -> Boolean
isExported (CST.ModuleHeader { exports: Nothing }) _ = true
isExported (CST.ModuleHeader { exports: Just exports }) pd =
  getName pd `elem` exports'
  where
  (CST.Wrapped { value: CST.Separated { head, tail } }) = exports
  exports' = exportToName <$> (head : map snd tail)

exportToName :: CST.Export Void -> String
exportToName = case _ of
  CST.ExportType (CST.Name { name: CST.Proper n }) _ -> n
  _ -> ""

getPursDef :: CST.Declaration Void -> Maybe PursDef
getPursDef = case _ of
  CST.DeclData
    { name: CST.Name { name: CST.Proper name }
    , vars: []
    }
    _ -> Just $ DefData (Name name)

  CST.DeclSignature (CST.Labeled { label, value }) ->
    let
      CST.Name { name: CST.Ident n } = label

    in
      case value of
        CST.TypeConstructor _ -> Just $ DefValue (Name n)
        _ -> Nothing

  CST.DeclNewtype _ _ (CST.Name { name: CST.Proper n }) _ ->
    Just $ DefNewtype (Name n)

  CST.DeclType
    { name: CST.Name { name: CST.Proper name }
    , vars: []
    }
    _
    _ -> Just $ DefType (Name name)
  _ -> Nothing

getName :: PursDef -> String
getName = case _ of
  DefData (Name n) -> n
  _ -> ""

readTextFile :: String -> TsBridgeGenM String
readTextFile path = FS.readTextFile UTF8 path
  # liftAffWithErr (const $ ErrReadFile path)

liftAffWithErr :: forall a. (Error -> TsBridgeGenError) -> Aff a -> TsBridgeGenM a
liftAffWithErr mkError ma = ma
  # try
  <#> lmap mkError
  # liftAff
  >>= liftEither

getPursModules :: Array Glob -> TsBridgeGenM (Array PursModule)
getPursModules globs = do
  paths <- getPaths globs
  sources <- for paths readTextFile
  cstModules <- for sources (liftEither <<< parseCstModule)
  pure $ getPursModule <$> cstModules

replaceComment :: String -> (String -> String) -> String -> String
replaceComment id f = R.replace'
  (R.unsafeRegex (regexStart <> regexMiddle <> regexEnd) R.noFlags)
  (unsafePartial matchFn)
  where
  regexStart = "({-GEN:" <> id <> "-})"
  regexMiddle = "([\\s\\S]*?)"
  regexEnd = "({-GEN:END-})"

  matchFn :: Partial => _
  matchFn _ [ (Just matchStart), (Just matchMiddle), (Just matchEnd) ] =
    matchStart <> "\n\n" <> f matchMiddle <> "\n\n" <> matchEnd