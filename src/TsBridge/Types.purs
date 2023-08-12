module TsBridge.Types
  ( AppError(..)
  , Name
  , PursModuleName
  , TsNameError(..)
  , class CapError
  , class CapThrow
  , mapErr
  , mkName
  , mkPursModuleName
  , printError
  , printName
  , toTsName
  , unsafeName
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import DTS as DTS
import Data.Foldable (fold, for_)
import Data.Generic.Rep (class Generic)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String as Str
import Data.String.CodeUnits (toCharArray)
import Data.String.CodeUnits as Char
import Data.String.Regex (Regex)
import Data.String.Regex as Reg
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Text.PrettyPrint.Leijen (Doc)
import Text.PrettyPrint.Leijen as P

data AppError
  = ErrInvalidPursModuleName String
  | ErrUnquantifiedTypeVariables (Set DTS.TsName)
  | AtModule String AppError
  | AtValue String AppError
  | AtType String AppError
  | ErrTsName TsNameError
  | ErrDuplicateIdentifier DTS.TsName

data TsNameError
  = ErrReserveredWord String
  | ErrInvalidBeginning String
  | ErrInvalidCharacter Char
  | ErrEmpty

newtype PursModuleName = UnsafePursModuleName String

newtype Name = UnsafeName String

class (MonadError AppError m) <= CapError m

class (MonadThrow AppError m) <= CapThrow m

mkName :: forall m. MonadThrow AppError m => String -> m Name
mkName s = do
  when (s == "") $
    throwError (ErrTsName ErrEmpty)

  when (Set.member s tsReservedWords) $
    throwError (ErrTsName $ ErrReserveredWord s)

  let
    head = Str.take 1 s
    tail = toCharArray $ Str.drop 1 s

  when (not $ Reg.test tsNameRegexFirst head) $
    throwError (ErrTsName $ ErrInvalidBeginning head)

  for_ tail \c ->
    when (not $ Reg.test tsNameRegexRest $ Char.singleton c) $
      throwError (ErrTsName $ ErrInvalidCharacter c)

  pure $ UnsafeName s

unsafeName :: String -> Name
unsafeName s = UnsafeName s

toTsName :: Name -> DTS.TsName
toTsName (UnsafeName n) = DTS.TsName n

printName :: Name -> String
printName (UnsafeName s) = s

mkPursModuleName :: forall m. MonadError AppError m => String -> m PursModuleName
mkPursModuleName s = do
  when (not $ Regex.test pursModuleNameRegex s) $
    throwError (ErrInvalidPursModuleName s)
  pure (UnsafePursModuleName s)

pursModuleNameRegex :: Regex
pursModuleNameRegex = unsafeRegex "^([A-Z][A-Za-z0-9_]*)(\\.[A-Z][A-Za-z0-9_]*)*$" noFlags

printError :: AppError -> Doc
printError = case _ of
  AtModule name err -> fold
    [ P.text ("At module `" <> name <> "`")
    , P.line
    , P.indent 2 $ printError err
    ]

  AtValue name err -> fold
    [ P.text ("At value `" <> name <> "`")
    , P.line
    , P.indent 2 $ printError err
    ]

  AtType name err -> fold
    [ P.text ("At type `" <> name <> "`")
    , P.line
    , P.indent 2 $ printError err
    ]

  ErrUnquantifiedTypeVariables vars ->
    P.text $ fold
      [ "Type variables"
      , " "
      , vars
          # Set.toUnfoldable
          <#> DTS.printTsName >>> (\s -> "`" <> s <> "`")
          # Str.joinWith ", "
      , " "
      , "are not behind a function. This is not possible in TypeScript. E.g. `Maybe a` needs to be exported as `Unit -> Maybe a`."
      ]
  ErrInvalidPursModuleName _ ->
    P.text "Invalid PureScript module name"

  ErrTsName err -> P.text case err of
    ErrEmpty -> "Identifier is empty"
    ErrReserveredWord s -> s <> " is a reserved word."
    ErrInvalidBeginning s -> "Identifer cannot start with `" <> s <> "`"
    ErrInvalidCharacter s -> "Identifier cannot contain `" <> Char.singleton s <> "`"

  ErrDuplicateIdentifier (DTS.TsName s) -> P.text ("Duplicate identifier: `" <> s <> "`")

mapErr :: forall e m a. MonadError e m => (e -> e) -> m a -> m a
mapErr f ma = catchError ma (f >>> throwError)

tsReservedWords :: Set String
tsReservedWords = Set.fromFoldable
  [ "break"
  , "case"
  , "catch"
  , "class"
  , "const"
  , "continue"
  , "debugger"
  , "default"
  , "delete"
  , "do"
  , "else"
  , "enum"
  , "export"
  , "extends"
  , "false"
  , "finally"
  , "for"
  , "function"
  , "if"
  , "import"
  , "in"
  , "instanceof"
  , "new"
  , "null"
  , "return"
  , "super"
  , "switch"
  , "this"
  , "throw"
  , "true"
  , "try"
  , "typeof"
  , "var"
  , "void"
  , "while"
  , "with"

  , "as"
  , "implements"
  , "interface"
  , "let"
  , "package"
  , "private"
  , "protected"
  , "public"
  , "static"
  , "yield"

  , "any"
  , "constructor"
  , "declare"
  , "get"
  , "module"
  , "require"
  , "set"
  , "symbol"
  , "type"
  , "from"
  , "of"

  , "undefined"
  ]

tsNameRegexFirst :: Regex
tsNameRegexFirst = unsafeRegex "[_$A-Za-z]" noFlags

tsNameRegexRest :: Regex
tsNameRegexRest = unsafeRegex "[_$A-Za-z0-9]" noFlags

derive instance Generic TsNameError _
derive instance Generic AppError _

derive instance Eq Name
derive instance Eq TsNameError
derive instance Eq AppError

derive instance Ord Name
derive instance Ord TsNameError
derive instance Ord AppError

instance Show TsNameError where
  show = genericShow

instance Show Name where
  show (UnsafeName x) = show x

instance Show AppError where
  show x = genericShow x
