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
import Data.Foldable (for_)
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
import Unsafe.Coerce (unsafeCoerce)

data AppError
  = ErrInvalidPursModuleName DefPursModuleName
  | ErrUnquantifiedTypeVariables (Set Name)
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
unsafeName = unsafeCoerce 1

toTsName :: Name -> DTS.TsName
toTsName = unsafeCoerce 1

mkPursModuleName :: forall m. MonadError AppError m => DefPursModuleName -> m PursModuleName
mkPursModuleName draft@(DefPursModuleName s) = do
  when (not $ Regex.test pursModuleNameRegex s) $
    throwError (ErrInvalidPursModuleName draft)
  pure (UnsafePursModuleName s)

pursModuleNameRegex :: Regex
pursModuleNameRegex = unsafeRegex "^([A-Z][a-z0-9_]*)(\\.[A-Z][a-z0-9_]*)*$" noFlags

printError :: AppError -> String
printError = unsafeCoerce 1

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
