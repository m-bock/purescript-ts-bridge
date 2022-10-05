module TsBridgeGen.Core where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (Json, jsonParser)
import Data.Array (catMaybes, elem, uncons, (:))
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as Str
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Undefined (undefined)
import Parsing (ParserT, runParser)
import Parsing.String (anyTill, eof, string) as P
import Parsing.String.Basic (intDecimal) as P
import PureScript.CST (RecoveredParserResult(..), parseImportDecl, parseModule) as CST
import PureScript.CST.Types (Declaration(..), Export(..), Foreign(..), Ident(..), ImportDecl(..), Labeled(..), Module(..), ModuleBody(..), ModuleHeader(..), ModuleName(..), Name(..), Proper(..), Separated(..), Wrapped(..)) as CST
import TsBridgeGen.Types (AppError(..), ErrorParseToJson(..), Glob(..), ModuleGlob(..), ModuleName(..), Name(..), PursDef(..), PursModule(..), SourcePosition(..))

parseCstModule :: String -> Either AppError (CST.Module Void)
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
  (getName pd # \(Name n) -> n) `elem` exports'
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
     --Just $ DefUnsupportedExport (Name name) "data type" 
      

  CST.DeclData
    { name: CST.Name { name: CST.Proper name } }
    _ -> Just $ DefUnsupportedInstAndExport (Name name) "data type with arguments"

  -- CST.DeclSignature (CST.Labeled { label, value }) ->
  --   let
  --     CST.Name { name: CST.Ident n } = label

  --   in
  --     case value of
  --       CST.TypeConstructor _ -> Just $ DefValue (Name n)
  --       _ -> Nothing

  CST.DeclSignature
    ( CST.Labeled
        { label: CST.Name { name: CST.Ident n }
        }
    ) -> Just $ DefUnsupportedExport (Name n) "value"

  CST.DeclNewtype
    { name: CST.Name { name: CST.Proper n }
    , vars: []
    }
    _
    _
    _ -> Just $ DefUnsupportedInstAndExport (Name n) "newtype"
  --Just $ DefNewtype (Name n)

  CST.DeclType
    { name: CST.Name { name: CST.Proper name }
    , vars: []
    }
    _
    _ -> Just $ DefUnsupportedExport (Name name) "type alias" -- Just $ DefType (Name name)

  CST.DeclType
    { name: CST.Name { name: CST.Proper name }
    }
    _
    _ -> Just $ DefUnsupportedExport (Name name) "type with arguments"

  CST.DeclForeign _ _
    ( CST.ForeignValue
        ( CST.Labeled
            { label: CST.Name { name: CST.Ident n }
            }
        )
    ) ->
    Just $ DefUnsupportedExport (Name n) "foreign import"

  CST.DeclForeign _ _
    ( CST.ForeignData _
        ( CST.Labeled
            { label: CST.Name { name: CST.Proper n }
            }
        )
    ) ->
    Just $ DefUnsupportedInstAndExport (Name n) "foreign import"

  _ -> Nothing

getName :: PursDef -> Name
getName = case _ of
  DefData n -> n
  DefData n -> n
  DefNewtype n -> n
  DefType n -> n
  DefValue n -> n
  DefUnsupportedInstAndExport n _ -> n
  DefUnsupportedExport n _ -> n

indexToSourcePos :: Int -> String -> Maybe SourcePosition
indexToSourcePos pos str = go 1 1 (Str.split (Pattern "\n") str)
  where
  go :: Int -> Int -> Array String -> _
  go line idx xs = case uncons xs of
    Just { head, tail } ->
      let
        len = Str.length head
      in
        if pos < idx + len then
          Just $ SourcePosition { line, column: (pos - idx) + 1 }
        else go (line + 1) (idx + len) tail
    Nothing -> Nothing

parseToJson :: String -> Either ErrorParseToJson Json
parseToJson s = jsonParser s
  # lmap f
  where
  f e = runParser e (parseJsonError s)
    # either (const $ Other e) identity

parseJsonError :: forall m. Monad m => String -> ParserT String m ErrorParseToJson
parseJsonError json = p1 <|> p2
  where
  p1 = do
    _ <- P.string "Unexpected end of JSON input"
    _ <- P.eof
    pure UnexpectedEndOfInput
  p2 = do
    _ <- P.string "Unexpected token "
    tok /\ _ <- P.anyTill $ P.string " "
    _ <- P.string "in JSON at position "
    i <- P.intDecimal
    _ <- P.eof
    pure $ UnexpectedTokenAtPos tok
      ( indexToSourcePos i json
          # fromMaybe (SourcePosition { line: 0, column: 0 })
      )

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

type ReplaceInstancesOpts =
  { include :: Array ModuleGlob
  , exclude :: Array ModuleGlob
  }

type ReplaceTsProgramOpts =
  { include :: Array ModuleGlob
  , exclude :: Array ModuleGlob
  }

type ReplaceImportsOpts =
  { }

