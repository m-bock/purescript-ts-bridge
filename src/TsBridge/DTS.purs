module TsBridge.DTS
  ( Error(..)
  , OSet(..)
  , PropModifiers
  , TsDeclVisibility(..)
  , TsDeclaration(..)
  , TsFilePath(..)
  , TsFnArg(..)
  , TsImport(..)
  , TsModule(..)
  , TsModuleAlias(..)
  , TsModuleFile(..)
  , TsModulePath(..)
  , TsName
  , TsNameError(..)
  , TsProgram(..)
  , TsQualName(..)
  , TsRecordField(..)
  , TsType(..)
  , TsTypeArgs(..)
  , TsTypeArgsQuant(..)
  , dtsFilePath
  , mapQuantifier
  , mkTsName
  , printError
  , printTsName
  , unsafeTsName
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Foldable (fold, for_)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.Set.Ordered as OSet
import Data.Show.Generic (genericShow)
import Data.String as Str
import Data.String.CodeUnits (toCharArray)
import Data.String.CodeUnits as Char
import Data.String.Regex (Regex)
import Data.String.Regex as Reg
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe as Regex

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Error
  = ErrUnquantifiedTypeVariables (Set TsName)
  | ErrIllegalSymbolName String
  | ErrTsName TsNameError
  | AtModule String Error
  | AtValue TsName Error

data TsNameError
  = ErrReserveredWord String
  | ErrInvalidBegining String
  | ErrInvalidCharacter Char
  | ErrEmpty

-- | Represents a subset of TypeScript type declarations
data TsDeclaration
  = TsDeclTypeDef TsName TsDeclVisibility (OSet TsName) TsType
  | TsDeclValueDef TsName TsDeclVisibility TsType
  | TsDeclComments (Array String)

data TsDeclVisibility = Public | Private

-- | Represents a subset of TypeScript types
data TsType
  = TsTypeNumber
  | TsTypeString
  | TsTypeBoolean
  | TsTypeNull
  | TsTypeArray TsType
  | TsTypeReadonlyArray TsType
  | TsTypeIntersection (Array TsType)
  | TsTypeUnion (Array TsType)
  | TsTypeRecord (Array TsRecordField)
  | TsTypeFunction TsTypeArgsQuant (Array TsFnArg) TsType
  | TsTypeConstructor TsQualName TsTypeArgs
  | TsTypeUniqueSymbol
  | TsTypeVar TsName
  | TsTypeTypelevelString String
  | TsTypeVoid

data TsModule = TsModule String (Set TsImport) (Array TsDeclaration)

data TsModuleFile = TsModuleFile TsFilePath TsModule

-- | A collection of TypeScript modules
data TsProgram = TsProgram (Map TsFilePath TsModule)

newtype Statements a = Statements a

data TsName = TsName String

newtype TsModuleAlias = TsModuleAlias String

data TsModulePath = TsModulePath String

data TsFilePath = TsFilePath String String

data TsImport = TsImport TsModuleAlias TsModulePath

data TsQualName = TsQualName (Maybe TsModuleAlias) TsName

newtype TsTypeArgsQuant = TsTypeArgsQuant (OSet TsName)

newtype TsTypeArgs = TsTypeArgs (Array TsType)

data TsFnArg = TsFnArg TsName TsType

data TsRecordField = TsRecordField String PropModifiers TsType

type PropModifiers =
  { readonly :: Boolean
  , optional :: Boolean
  }

-------------------------------------------------------------------------------

unsafeTsName :: String -> TsName
unsafeTsName = TsName

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
tsNameRegexFirst = Regex.unsafeRegex "[_$A-Za-z]" noFlags

tsNamerRegexRest :: Regex
tsNamerRegexRest = Regex.unsafeRegex "[_$A-Za-z0-9]" noFlags

mkTsName :: forall m. MonadThrow Error m => String -> m TsName
mkTsName s = do
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
    when (not $ Reg.test tsNamerRegexRest $ Char.singleton c) $
      throwError (ErrTsName $ ErrInvalidCharacter c)

  pure $ TsName s

dtsFilePath :: String -> TsFilePath
dtsFilePath x = TsFilePath (x <> "/index") "d.ts"

mapQuantifier :: (OSet TsName -> OSet TsName) -> TsType -> TsType
mapQuantifier f = case _ of
  TsTypeNumber -> TsTypeNumber
  TsTypeString -> TsTypeString
  TsTypeBoolean -> TsTypeBoolean
  TsTypeNull -> TsTypeNull
  TsTypeArray x -> TsTypeArray $ mapQuantifier f x
  TsTypeReadonlyArray x -> TsTypeReadonlyArray $ mapQuantifier f x
  TsTypeIntersection xs -> TsTypeIntersection
    (mapQuantifier f <$> xs)
  TsTypeUnion xs -> TsTypeUnion
    (mapQuantifier f <$> xs)
  TsTypeRecord x -> TsTypeRecord $ goTsRecordField <$> x
  TsTypeFunction x y z -> TsTypeFunction
    (goTsTypeArgsQuant x)
    (goTsFnArg <$> y)
    (mapQuantifier f z)
  TsTypeConstructor x y -> TsTypeConstructor
    (goTsQualName x)
    (goTsTypeArgs y)
  TsTypeUniqueSymbol -> TsTypeUniqueSymbol
  TsTypeVar x -> TsTypeVar $ goTsName x
  TsTypeVoid -> TsTypeVoid
  TsTypeTypelevelString x -> TsTypeTypelevelString x

  where
  goTsRecordField (TsRecordField x y z) = TsRecordField
    (goTsName x)
    (goPropModifiers y)
    (mapQuantifier f z)

  goTsTypeArgsQuant (TsTypeArgsQuant oset) = TsTypeArgsQuant $ f oset

  goTsTypeArgs (TsTypeArgs x) = TsTypeArgs $ mapQuantifier f <$> x

  goTsFnArg (TsFnArg x y) = TsFnArg (goTsName x) (mapQuantifier f y)

  goTsQualName = identity

  goTsName = identity

  goPropModifiers = identity

printTsName :: TsName -> String
printTsName (TsName n) = n

printError :: Error -> String
printError = case _ of
  AtModule name err -> Str.joinWith "\n"
    [ "At module " <> name
    , printError err
    ]
  AtValue name err -> Str.joinWith "\n"
    [ "At Value " <> printTsName name
    , printError err
    ]
  ErrUnquantifiedTypeVariables vars ->
    fold
      [ "Type variables"
      , " "
      , vars # Set.toUnfoldable <#> printTsName # Str.joinWith ", "
      , " "
      , "are not behind a function. This is not possible in TypeScript. E.g. `Maybe a` needs to be exported as `Unit -> Maybe a`."
      ]
  ErrIllegalSymbolName _ -> ""
  ErrTsName err -> case err of
    ErrEmpty -> "Identifier is empty"
    ErrReserveredWord s -> s <> " is a reserved word."
    ErrInvalidBegining s -> "Identifer cannot start with `" <> s <> "`"
    ErrInvalidCharacter s -> "Identifier cannot contain `" <> Char.singleton s <> "`"

-------------------------------------------------------------------------------
-- Wrap
-------------------------------------------------------------------------------

newtype OSet a = OSet (OSet.OSet a)

derive instance Newtype (OSet a) _

derive newtype instance Eq a => Semigroup (OSet a)

instance Ord a => Ord (OSet a) where
  compare (OSet o1) (OSet o2) = toArray o1 `compare` toArray o2
    where
    toArray = OSet.toUnfoldable :: _ -> Array _

instance Eq a => Eq (OSet a) where
  eq (OSet o1) (OSet o2) = toArray o1 `eq` toArray o2
    where
    toArray = OSet.toUnfoldable :: _ -> Array _

instance Eq a => Monoid (OSet a) where
  mempty = OSet $ OSet.empty

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

derive instance Eq TsRecordField
derive instance Eq TsFnArg
derive instance Eq TsTypeArgs
derive instance Eq TsImport
derive instance Eq TsModuleAlias
derive instance Eq TsQualName
derive instance Eq TsTypeArgsQuant
derive instance Eq TsDeclaration
derive instance Eq TsType
derive instance Eq TsName
derive instance Eq TsModulePath
derive instance Eq TsFilePath
derive instance Eq TsDeclVisibility
derive instance Eq Error
derive instance Eq TsNameError

derive instance Ord TsFnArg
derive instance Ord TsRecordField
derive instance Ord TsTypeArgs
derive instance Ord TsImport
derive instance Ord TsModuleAlias
derive instance Ord TsQualName
derive instance Ord TsTypeArgsQuant
derive instance Ord TsDeclaration
derive instance Ord TsType
derive instance Ord TsName
derive instance Ord TsModulePath
derive instance Ord TsFilePath
derive instance Ord TsDeclVisibility

derive instance Generic Error _
derive instance Generic TsNameError _

instance Show TsName where
  show (TsName name) = show name

instance Show TsNameError where
  show = genericShow

instance Show Error where
  show x = genericShow x
