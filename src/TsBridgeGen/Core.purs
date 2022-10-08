module TsBridgeGen.Core where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (Json, jsonParser)
import Data.Array (catMaybes, elem, uncons, (:))
import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as Str
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Undefined (undefined)
import Parsing (ParserT, Position(..), runParser)
import Parsing.String (anyTill, eof, string) as P
import Parsing.String.Basic (intDecimal) as P
import PureScript.CST (RecoveredParserResult(..), parseImportDecl, parseModule) as CST
import PureScript.CST.Types (DataCtor, Declaration(..), Export(..), Foreign(..), Ident(..), ImportDecl(..), Labeled(..), Module(..), ModuleBody(..), ModuleHeader(..), ModuleName(..), Name(..), Proper(..), QualifiedName(..), Separated(..), Type(..), TypeVarBinding(..), Wrapped(..)) as CST
import PureScript.CST.Types (Separated(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import TsBridgeGen.Types (AppError(..), ErrorParseToJson(..), Glob(..), ModuleGlob(..), ModuleName(..), Name(..), PursDef(..), PursModule(..), SourcePosition(..), UnsupportedScope(..))

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

allVarsNotHigherKinded :: forall a. Array a -> Array (CST.DataCtor Void) -> Boolean
allVarsNotHigherKinded _ _  = true

isSimpleType :: CST.Type Void -> Either String Unit
isSimpleType = case _ of
  CST.TypeApp (CST.TypeVar _) _ -> Left "Higher kinded type"
  CST.TypeRow _ -> Left "Row type"
  CST.TypeRecord _ -> undefined
  CST.TypeForall _ _ _ _ -> Left "Existential type"
  CST.TypeKinded _ _ _ -> undefined
  CST.TypeApp t ts -> traverse isSimpleType (t `NEA.cons` ts) <#> const unit
  CST.TypeOp _ _ -> undefined
  CST.TypeArrow _ _ _ -> undefined
  CST.TypeConstrained _ _ _ -> Left "Constrained type"
  CST.TypeParens _ -> undefined
  CST.TypeVar _ -> undefined
  CST.TypeConstructor _  -> undefined
  CST.TypeWildcard _ -> Right unit
  CST.TypeHole _ -> Left "type hole"
  CST.TypeString _ _ -> Left "TL string"
  CST.TypeInt _ _ _ -> Left "TL int"
  CST.TypeOpName _ -> undefined
  CST.TypeArrowName _ -> undefined
  CST.TypeError _ -> Right unit

getPursDef :: CST.Declaration Void -> Maybe PursDef
getPursDef = case _ of
  CST.DeclData
    { name: CST.Name { name: CST.Proper name }
    , vars
    }
    (Just (Tuple _ (Separated {head, tail}))) | allVarsNotHigherKinded vars (head : map snd tail) -> case vars # traverse getTypeVar <#> DefData (Name name) of
    Nothing -> Just $ DefUnsupported (Name name) BothExportAndInstance "data type with unsupported type arguments"
    Just x -> Just x
    where

    getTypeVar = case _ of
      CST.TypeVarName (CST.Name { name: CST.Ident s }) | Str.length s == 1 -> Just $ Name s
      CST.TypeVarKinded
        ( CST.Wrapped
            { value: CST.Labeled
                { label:
                    CST.Name { name: CST.Ident s }
                , value: CST.TypeConstructor (CST.QualifiedName { name: CST.Proper "Type" })
                }
            }
        ) | Str.length s == 1 -> Just $ Name s
      _ -> Nothing

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
    ) ->  Just $ DefUnsupported (Name n) JustExport "value" -- Just $ DefValue (Name n)

  CST.DeclNewtype
    { name: CST.Name { name: CST.Proper n }
    , vars: []
    }
    _
    _
    _ -> Just $ DefUnsupported (Name n) BothExportAndInstance "newtype" -- Just $ DefNewtype (Name n)
  --Just $ DefNewtype (Name n)

  CST.DeclType
    { name: CST.Name { name: CST.Proper name }
    , vars: []
    }
    _
    _ -> Just $ DefUnsupported (Name name) JustExport "type alias"  -- Just $ DefType (Name name) -- Just $ DefType (Name name)

  CST.DeclType
    { name: CST.Name { name: CST.Proper name }
    }
    _
    _ -> Just $ DefUnsupported (Name name) JustExport "type alias with arguments"

  CST.DeclForeign _ _
    ( CST.ForeignValue
        ( CST.Labeled
            { label: CST.Name { name: CST.Ident name }
            }
        )
    ) ->
    Just $ DefUnsupported (Name name) JustExport "foreign"

  CST.DeclForeign _ _
    ( CST.ForeignData _
        ( CST.Labeled
            { label: CST.Name { name: CST.Proper name}
            }
        )
    ) ->
    Just $ DefUnsupported (Name name) JustExport "foreign"

  _ -> Nothing

getName :: PursDef -> Name
getName = case _ of
  DefData n _ -> n
  DefNewtype n -> n
  DefType n -> n
  DefValue n -> n
  DefUnsupported n _ _ -> n

indexToSourcePos :: String -> Int -> Maybe SourcePosition
indexToSourcePos _ i | i < 0 = Nothing
indexToSourcePos str pos = go 0 0 (Str.split (Pattern "\n") str)
  where
  go :: Int -> Int -> Array String -> _
  go line idx xs = case uncons xs of
    Just { head, tail } ->
      let
        len = Str.length head
      in
        if pos < idx + len then
          Just $ SourcePosition { line, column: pos - idx }
        else go (line + 1) (idx + len) tail
    Nothing -> Nothing

positionToSourcePosition :: Position -> SourcePosition
positionToSourcePosition (Position { line, column }) =
  SourcePosition { line: line - 1, column: column - 1 }

parseToJson :: String -> Either ErrorParseToJson Json
parseToJson s = jsonParser s
  # lmap f
  where
  f e = runParser e parseJsonError
    # either (const $ Other e) identity

parseJsonError :: forall m. Monad m => ParserT String m ErrorParseToJson
parseJsonError = p1 <|> p2
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
    pure $ UnexpectedTokenAtPos tok i

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
  {}

