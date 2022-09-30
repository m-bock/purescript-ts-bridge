module TsBridgeGen.Core where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Writer (lift)
import Data.Argonaut (class DecodeJson, decodeJson, jsonParser, printJsonDecodeError)
import Data.Array (catMaybes, elem, (:))
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either, either)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as Str
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Undefined (undefined)
import Debug (spy)
import Parsing (fail) as P
import Parsing.String (anyTill, string) as P
import Parsing.String.Replace (replaceT) as P
import PureScript.CST (RecoveredParserResult(..), parseImportDecl, parseModule) as CST
import PureScript.CST.Types (Declaration(..), Export(..), Ident(..), ImportDecl(..), Labeled(..), Module(..), ModuleBody(..), ModuleHeader(..), ModuleName(..), Name(..), Proper(..), Separated(..), Type(..), Wrapped(..)) as CST
import TsBridgeGen.Print (genInstances, printDecls, printImports, runImportWriterM)
import TsBridgeGen.Types (Import(..), ModuleName(..), Name(..), PursDef(..), PursModule(..), TsBridgeGenError(..))

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

replaceComment :: forall m a. MonadRec m => DecodeJson a => String -> (a -> String -> m String) -> String -> m String
replaceComment id f i = P.replaceT i do
  genStartOpen <- P.string ("{-GEN:" <> id <> "\n")
  let _ = spy "x" "x"
  json /\ genStartClose <- P.anyTill (P.string "\n-}\n")
  let _ = spy "json" json
  data_ <- decode json
  let _ = spy "z" "z"
  content /\ genEnd <- P.anyTill (P.string "\n{-GEN:END-}")
  newContent <- lift $ f data_ content
  pure (genStartOpen <> json <> genStartClose <> "\n" <> newContent <> "\n" <> genEnd)
  where
  decode str = str
    # jsonParser
    # spy "a"
    >>= decodeJson >>> lmap printJsonDecodeError
    # either P.fail pure

parseUserImports :: String -> Maybe (Set String)
parseUserImports s = s
  # Str.split (Pattern "\n")
  >>= f
  # sequence
  <#> Set.fromFoldable
  where
  f "" = []
  f s' = case CST.parseImportDecl s' of
    CST.ParseSucceeded (CST.ImportDecl decl) -> case decl of
      { qualified: Just (Tuple _ (CST.Name { name: CST.ModuleName mn })) } -> case Str.stripPrefix (Pattern "Auto.") mn of
        Just _ -> []
        Nothing -> [ pure s' ]
      _ -> [ pure s' ]
    _ -> [ Nothing ]

patchModulesFile :: Array PursModule -> String -> String
patchModulesFile = undefined

type ReplaceInstancesOpts = {}

type ReplaceImportsOpts = {}

patchClassFile :: Array PursModule -> String -> String
patchClassFile defs file = file
  # replaceComment "instances"
      ( \(_ :: ReplaceInstancesOpts) _ -> do
          instances <- defs # genInstances
          pure $ printDecls instances
      )
  # runImportWriterM
  #
    ( \(file' /\ { imports }) -> file'
        # replaceComment "imports"
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
  # un Identity