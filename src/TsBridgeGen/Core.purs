module TsBridgeGen.Core where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (Json, jsonParser)
import Data.Array (catMaybes, drop, elem, range, uncons, (:))
import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Semigroup.Foldable (foldl1)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..))
import Data.String as Str
import Data.Traversable (sequence, traverse, traverse_)
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Parsing (ParserT, Position(..), runParser)
import Parsing.String (anyTill, eof, string) as P
import Parsing.String.Basic (intDecimal) as P
import PureScript.CST (RecoveredParserResult(..), parseImportDecl, parseModule) as CST
import PureScript.CST.Types (DataCtor, Declaration(..), Export(..), Foreign(..), Ident(..), ImportDecl(..), Labeled(..), Module(..), ModuleBody(..), ModuleHeader(..), ModuleName(..), Name(..), Operator(..), Proper(..), QualifiedName(..), Row(..), Separated(..), Type(..), TypeVarBinding(..), Wrapped(..)) as CST
import PureScript.CST.Types (Separated(..))
import TsBridgeGen.Types (AppError(..), ErrorParseToJson(..), ModuleGlob, ModuleName(..), Name(..), PursDef(..), PursModule(..), SourcePosition(..), TypeAnn(..), UnsupportedScope(..))

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

isSimpleADT :: Array (CST.DataCtor Void) -> Either String Unit
isSimpleADT xs = xs >>= (unwrap >>> _.fields) # traverse_ isSimpleType

isSimpleType :: CST.Type Void -> Either String Unit
isSimpleType = case _ of
  CST.TypeApp (CST.TypeVar _) _ -> Left "Higher kinded type"
  CST.TypeInt _ _ _ -> Left "TL int"
  CST.TypeVar _ -> pure unit
  CST.TypeHole _ -> Left "type hole"
  CST.TypeRow _ -> Left "Row type"
  CST.TypeForall _ _ _ _ -> Left "Existential type"
  CST.TypeString _ _ -> Left "TL string"
  CST.TypeError _ -> Right unit
  CST.TypeKinded _ _ _ -> Left "Kinded" -- ??
  CST.TypeArrowName _ -> Left "arrow name" -- pure unit
  CST.TypeOpName (CST.QualifiedName { name: CST.Operator "~>" }) -> Left "Natural transformation"
  CST.TypeOpName _ -> pure unit
  CST.TypeConstructor _ -> pure unit
  CST.TypeWildcard _ -> Right unit
  CST.TypeConstrained _ _ _ -> Left "Constrained type"
  CST.TypeApp t ts -> traverse_ isSimpleType (t `NEA.cons` ts)
  CST.TypeParens (CST.Wrapped { value }) -> isSimpleType value
  CST.TypeRecord (CST.Wrapped { value }) -> isSimpleRow value
  CST.TypeOp t ts -> traverse_ isSimpleType (t `NEA.cons` (snd <$> ts))
  CST.TypeArrow t1 _ t2 -> do
    isSimpleType t1
    isSimpleType t2
  where
  isSimpleRow (CST.Row { labels, tail: Nothing }) = case labels of
    Just (CST.Separated { head, tail }) -> traverse_ isSimpleType $ _.value <<< unwrap <$> (head : map snd tail)
    Nothing -> pure unit
  isSimpleRow (CST.Row {}) = Left "Open Record"

data UnsupportedTypeErr
  = ErrRowKind
  | ErrForall
  | ErrTLString
  | ErrKinded
  | ErrUnsupported String
  | ErrConstrained
  | ErrOpenRow

derive instance Generic UnsupportedTypeErr _

instance Show UnsupportedTypeErr where
  show = genericShow

countTypeArgs :: CST.Type Void -> Either UnsupportedTypeErr Int
countTypeArgs = go 0
  where
  go i = case _ of
    CST.TypeArrow
      ( CST.TypeConstructor
          (CST.QualifiedName { module: Nothing, name: CST.Proper "Type" })
      )
      _
      t -> go (i + 1) t
    CST.TypeConstructor
      (CST.QualifiedName { module: Nothing, name: CST.Proper "Type" }) -> pure i
    _ -> throwError $ ErrUnsupported "kind"

typeToTypeAnn' :: CST.Type Void -> Either UnsupportedTypeErr TypeAnn
typeToTypeAnn' = case _ of
  CST.TypeForall _ _ _ t -> typeToTypeAnn t
  x -> typeToTypeAnn x

typeToTypeAnn :: CST.Type Void -> Either UnsupportedTypeErr TypeAnn
typeToTypeAnn = case _ of
  CST.TypeInt _ _ _ -> blank
  CST.TypeVar (CST.Name { name: CST.Ident n }) -> pure $ TypeAnnId (Just $ Name n)
  CST.TypeHole _ -> blank
  CST.TypeRow _ -> throwError ErrRowKind
  CST.TypeForall _ _ _ _ -> throwError ErrForall
  CST.TypeString _ _ -> throwError ErrTLString
  CST.TypeError _ -> blank
  CST.TypeKinded _ _ _ -> throwError ErrKinded
  CST.TypeArrowName _ -> throwError $ ErrUnsupported "TypeArrowName"
  CST.TypeOpName _ -> throwError $ ErrUnsupported "TypeOpName"
  CST.TypeConstructor _ -> blank
  CST.TypeWildcard _ -> blank
  CST.TypeConstrained _ _ _ -> throwError ErrConstrained
  CST.TypeArrow t1 _ t2 -> TypeAnnFn <$> typeToTypeAnn t1 <*> typeToTypeAnn t2
  CST.TypeRecord (CST.Wrapped { value }) -> throwError $ ErrUnsupported "TypeRecord" -- typeToTypeAnnRow value
  CST.TypeParens (CST.Wrapped { value }) -> typeToTypeAnn value
  CST.TypeApp t ts -> traverse typeToTypeAnn (t `NEA.cons` ts)
    <#> foldl1 TypeAnnApp
  CST.TypeOp t ts -> traverse typeToTypeAnn (t `NEA.cons` (snd <$> ts))
    <#> foldl1 TypeAnnApp
  where
  blank = pure $ TypeAnnId Nothing

-- typeToTypeAnnRow (CST.Row { labels, tail: Nothing }) = case labels of
--   Just (CST.Separated { head, tail }) -> traverse_ isSimpleType $ _.value <<< unwrap <$> (head : map snd tail)
--   Nothing -> pure unit
-- isSimpletypeToTypeAnnRowRow (CST.Row {}) = throwError ErrOpenRow

getPursDef :: CST.Declaration Void -> Maybe PursDef
getPursDef = case _ of
  CST.DeclData
    { name: CST.Name { name: CST.Proper name }
    , vars
    }
    ctors -> Just $
    f case ctors of
      Nothing -> []
      (Just (Tuple _ (Separated { head, tail }))) -> (head : map snd tail)

    where
    f x = case isSimpleADT x of
      Left e -> DefUnsupported (Name name) BothExportAndInstance e
      Right _ -> vars
        # traverse getTypeVar
        <#> DefData (Name name)
        # fromMaybe (DefUnsupported (Name name) BothExportAndInstance "data type with unsupported type arguments")

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
        , value
        }
    ) -> Just $
    case typeToTypeAnn' value of
      Left e -> DefUnsupported (Name n) JustExport ("unsupported value: " <> show e)
      Right typeAnn -> DefValue (Name n) (Just $ typeAnn)

  CST.DeclNewtype
    { name: CST.Name { name: CST.Proper n }
    , vars
    }
    _
    _
    value -> Just $
    case isSimpleType value of
      Left e -> DefUnsupported (Name n) JustExport ("unsupported newtype: " <> e)
      Right _ -> vars
        # traverse getTypeVar
        <#> DefNewtype (Name n)
        # fromMaybe (DefUnsupported (Name n) BothExportAndInstance "type with unsupported type arguments")

  CST.DeclType
    { name: CST.Name { name: CST.Proper name }
    , vars: []
    }
    _
    _ -> Just $ DefUnsupported (Name name) JustExport "type alias" -- Just $ DefType (Name name) -- Just $ DefType (Name name)

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
            { label: CST.Name { name: CST.Proper name }
            , value
            }
        )
    ) ->
    Just $ case isSimpleType value of
      Left e -> DefUnsupported (Name name) JustExport ("unsupported foreign data import: " <> e)
      Right _ -> case countTypeArgs value of
        Left e -> DefUnsupported (Name name) JustExport ("unsupported foreign data import: " <> show e)
        Right i -> DefData (Name name) (range 0 i # drop 1 <#> show >>> ("a" <> _) >>> Name)
  _ -> Nothing

getTypeVar = case _ of
  CST.TypeVarName (CST.Name { name: CST.Ident s }) -> Just $ Name s
  CST.TypeVarKinded
    ( CST.Wrapped
        { value: CST.Labeled
            { label:
                CST.Name { name: CST.Ident s }
            , value: CST.TypeConstructor (CST.QualifiedName { name: CST.Proper "Type" })
            }
        }
    ) -> Just $ Name s
  _ -> Nothing

getName :: PursDef -> Name
getName = case _ of
  DefData n _ -> n
  DefNewtype n _ -> n
  DefType n -> n
  DefValue n _ -> n
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

