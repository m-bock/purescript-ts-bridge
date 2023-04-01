module TsBridge.Types
  ( AppError(..)
  , DefName(..)
  , DefPursModuleName(..)
  , Name
  , PursModuleName
  , TsNameError(..)
  , class CapError
  , class CapThrow
  , mkName
  , mkPursModuleName
  , printError
  , toTsName
  , unsafeName
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
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

data AppError
  = ErrInvalidPursModuleName DefPursModuleName
  | ErrUnquantifiedTypeVariables (Set DTS.TsName)
  | AtModule String AppError
  | AtValue String AppError
  | ErrTsName TsNameError

data TsNameError
  = ErrReserveredWord String
  | ErrInvalidBegining String
  | ErrInvalidCharacter Char
  | ErrEmpty

newtype DefPursModuleName = DefPursModuleName String

newtype PursModuleName = UnsafePursModuleName String

newtype DefName = DefName String

newtype Name = UnsafeName String

class (MonadError AppError m) <= CapError m

class (MonadThrow AppError m) <= CapThrow m

mkName :: forall m. MonadThrow AppError m => DefName -> m Name
mkName (DefName s) = do
  when (s == "") $
    throwError (ErrTsName ErrEmpty)

  when (Set.member s tsReservedWords) $
    throwError (ErrTsName $ ErrReserveredWord s)

  let
    head = Str.take 1 s
    tail = toCharArray $ Str.drop 1 s

  when (not $ Reg.test tsNameRegexFirst head) $
    throwError (ErrTsName $ ErrInvalidBegining head)

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

mkPursModuleName :: forall m. MonadError AppError m => DefPursModuleName -> m PursModuleName
mkPursModuleName draft@(DefPursModuleName s) = do
  when (not $ Regex.test pursModuleNameRegex s) $
    throwError (ErrInvalidPursModuleName draft)
  pure (UnsafePursModuleName s)

pursModuleNameRegex :: Regex
pursModuleNameRegex = unsafeRegex "^([A-Z][a-z0-9_]*)(\\.[A-Z][a-z0-9_]*)*$" noFlags

printError :: AppError -> String
printError = case _ of
  AtModule name err -> Str.joinWith "\n"
    [ "At module " <> name
    , printError err
    ]
  AtValue name err -> Str.joinWith "\n"
    [ "At Value " <> name
    , printError err
    ]
  ErrUnquantifiedTypeVariables vars ->
    fold
      [ "Type variables"
      , " "
      , vars # Set.toUnfoldable <#> DTS.printTsName # Str.joinWith ", "
      , " "
      , "are not behind a function. This is not possible in TypeScript. E.g. `Maybe a` needs to be exported as `Unit -> Maybe a`."
      ]
  ErrInvalidPursModuleName _ -> ""
  ErrTsName err -> case err of
    ErrEmpty -> "Identifier is empty"
    ErrReserveredWord s -> s <> " is a reserved word."
    ErrInvalidBegining s -> "Identifer cannot start with `" <> s <> "`"
    ErrInvalidCharacter s -> "Identifier cannot contain `" <> Char.singleton s <> "`"

tsReservedWords :: Set String
tsReservedWords = Set.fromFoldable
  [ "instanceof"
  , "typeof"
  , "break"
  , "do"
  , "new"
  , "var"
  , "case"
  , "else"
  , "return"
  , "void"
  , "catch"
  , "finally"
  , "continue"
  , "for"
  , "switch"
  , "while"
  , "this"
  , "with"
  , "debugger"
  , "function"
  , "throw"
  , "default"
  , "if"
  , "try"
  , "delete"
  , "in"
  ]

tsNameRegexFirst :: Regex
tsNameRegexFirst = unsafeRegex "[_$A-Za-z]" noFlags

tsNameRegexRest :: Regex
tsNameRegexRest = unsafeRegex "[_$A-Za-z0-9]" noFlags

derive instance Generic DefPursModuleName _
derive instance Generic TsNameError _
derive instance Generic AppError _

derive instance Eq DefPursModuleName
derive instance Eq Name
derive instance Eq TsNameError
derive instance Eq AppError

derive instance Ord DefPursModuleName
derive instance Ord Name
derive instance Ord TsNameError
derive instance Ord AppError

instance Show DefPursModuleName where
  show = genericShow

instance Show TsNameError where
  show = genericShow

instance Show Name where
  show (UnsafeName x) = show x

instance Show AppError where
  show x = genericShow x
