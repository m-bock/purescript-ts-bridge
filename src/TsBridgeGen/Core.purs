module TsBridgeGen.Core where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (class MonadError, catchError, liftEither, throwError, try)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Writer (lift)
import Data.Argonaut (class DecodeJson, Json, decodeJson, jsonParser, printJsonDecodeError)
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
import Node.Path (FilePath)
import Parsing (ParserT, Position(..), fail, position, runParser)
import Parsing (fail) as P
import Parsing.String (anyTill, eof, string) as P
import Parsing.String.Basic (intDecimal) as P
import Parsing.String.Replace (replaceT) as P
import PureScript.CST (RecoveredParserResult(..), parseImportDecl, parseModule) as CST
import PureScript.CST.Types (Declaration(..), Export(..), Ident(..), ImportDecl(..), Labeled(..), Module(..), ModuleBody(..), ModuleHeader(..), ModuleName(..), Name(..), Proper(..), Separated(..), Type(..), Wrapped(..)) as CST
import TsBridgeGen.Monad (class MonadLog, log)
import TsBridgeGen.Print (genInstances, printDecls, printImports, runImportWriterT)
import TsBridgeGen.Types (AppError(..), AppLog(..), ErrorParseToJson(..), Import(..), ModuleName(..), Name(..), PursDef(..), PursModule(..), SourcePosition(..))

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

replaceComment'
  :: forall m a
   . MonadRec m
  => MonadError AppError m
  => DecodeJson a
  => String
  -> (a -> String -> m String)
  -> String
  -> m String
replaceComment' id f i = P.replaceT i do
  genStartOpen <- P.string ("{-GEN:" <> id <> "\n")
  json /\ genStartClose <- P.anyTill (P.string "\n-}\n")
  _ <- lift $ throwError $ ErrLiteral "tada"
  data_ <- decode json
  content /\ genEnd <- P.anyTill (P.string "\n{-GEN:END-}")
  newContent <- lift $ f data_ content
  pure (genStartOpen <> json <> genStartClose <> "\n" <> newContent <> "\n" <> genEnd)
  where
  decode str = str
    # jsonParser
    >>= decodeJson >>> lmap printJsonDecodeError
    # either P.fail pure

replaceComment
  :: forall m a
   . MonadRec m
  => MonadError AppError m
  => MonadLog AppLog m
  => DecodeJson a
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

  let sourcePos = SourcePosition {line: pos.line, column: pos.column}
  
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

  pure (genStartOpen <> json <> genStartClose <> "\n" <> newContent <> "\n" <> genEnd)
  where
  decode str = str
    # parseToJson
    # lmap ErrParseToJson
    >>= decodeJson >>> lmap ErrParseToData

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

patchModulesFile :: Array PursModule -> String -> String
patchModulesFile = undefined

type ReplaceInstancesOpts = {}

type ReplaceImportsOpts = {}

patchClassFile
  :: forall m
   . MonadRec m
  => MonadLog AppLog m
  => MonadError AppError m
  => FilePath
  -> Array PursModule
  -> String
  -> m String
patchClassFile path defs file = file
  # replaceComment path "instances"
      ( \(_ :: ReplaceInstancesOpts) _ -> do
          instances <- defs # genInstances
          pure $ printDecls instances
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

