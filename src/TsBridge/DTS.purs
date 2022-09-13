module TsBridge.DTS
  ( PropModifiers
  , TsDeclaration(..)
  , TsFilePath(..)
  , TsFnArg(..)
  , TsImport(..)
  , TsModule(..)
  , TsModuleAlias(..)
  , TsModuleFile(..)
  , TsModulePath(..)
  , TsName(..)
  , TsProgram(..)
  , TsQualName(..)
  , TsRecordField(..)
  , TsType(..)
  , TsTypeArgs(..)
  , TsTypeArgsQuant(..)
  , dtsFilePath
  ) where

import Prelude

import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Set (Set)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data TsDeclaration = TsDeclTypeDef TsName (Array TsName) TsType

data TsType
  = TsTypeNumber
  | TsTypeString
  | TsTypeBoolean
  | TsTypeArray TsType
  | TsTypeRecord (Array TsRecordField)
  | TsTypeFunction TsTypeArgsQuant (Array TsFnArg) TsType
  | TsTypeConstructor TsQualName TsTypeArgs
  | TsTypeUniqueSymbol
  | TsTypeVar TsName

data TsModule = TsModule (Set TsImport) (Array TsDeclaration)

data TsModuleFile = TsModuleFile TsFilePath TsModule

data TsProgram = TsProgram (Map TsFilePath TsModule)

newtype Statements a = Statements a

data TsName = TsName String

newtype TsModuleAlias = TsModuleAlias String

data TsModulePath = TsModulePath String

data TsFilePath = TsFilePath String String

data TsImport = TsImport TsModuleAlias TsModulePath

data TsQualName = TsQualName (Maybe TsModuleAlias) TsName

newtype TsTypeArgsQuant = TsTypeArgsQuant (Set TsName)

newtype TsTypeArgs = TsTypeArgs (Array TsType)

data TsFnArg = TsFnArg TsName TsType

data TsRecordField = TsRecordField TsName PropModifiers TsType

type PropModifiers =
  { readonly :: Boolean
  , optional :: Boolean
  }

-------------------------------------------------------------------------------
-- Class / Eq
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

-------------------------------------------------------------------------------
-- Class / Ord
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------

dtsFilePath :: String -> TsFilePath
dtsFilePath x = TsFilePath x "d.ts"