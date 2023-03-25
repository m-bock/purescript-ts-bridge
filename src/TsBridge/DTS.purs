module TsBridge.DTS
  ( PropModifiers
  , TsDeclVisibility(..)
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
  , TsBridge_DTS_Wrap(..)
  , dtsFilePath
  , mapQuantifier
  ) where

import Prelude

import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, over, wrap)
import Data.Set (Set)
import Data.Set.Ordered (OSet)
import Data.Set.Ordered as OSet

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Represents a subset of TypeScript type declarations
data TsDeclaration
  = TsDeclTypeDef TsName TsDeclVisibility (TsBridge_DTS_Wrap (OSet TsName)) TsType
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

newtype TsTypeArgsQuant = TsTypeArgsQuant (TsBridge_DTS_Wrap (OSet TsName))

newtype TsTypeArgs = TsTypeArgs (Array TsType)

data TsFnArg = TsFnArg TsName TsType

data TsRecordField = TsRecordField TsName PropModifiers TsType

type PropModifiers =
  { readonly :: Boolean
  , optional :: Boolean
  }

-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------

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

  goTsTypeArgsQuant (TsTypeArgsQuant oset) = TsTypeArgsQuant $ over wrap f oset

  goTsTypeArgs (TsTypeArgs x) = TsTypeArgs $ mapQuantifier f <$> x

  goTsFnArg (TsFnArg x y) = TsFnArg (goTsName x) (mapQuantifier f y)

  goTsQualName = identity

  goTsName = identity

  goPropModifiers = identity

-------------------------------------------------------------------------------
-- Wrap
-------------------------------------------------------------------------------

newtype TsBridge_DTS_Wrap a = TsBridge_DTS_Wrap a

derive instance Newtype (TsBridge_DTS_Wrap a) _

derive newtype instance Eq a => Semigroup (TsBridge_DTS_Wrap (OSet a))

instance Ord a => Ord (TsBridge_DTS_Wrap (OSet a)) where
  compare (TsBridge_DTS_Wrap o1) (TsBridge_DTS_Wrap o2) = toArray o1 `compare` toArray o2
    where
    toArray = OSet.toUnfoldable :: _ -> Array _

instance Eq a => Eq (TsBridge_DTS_Wrap (OSet a)) where
  eq (TsBridge_DTS_Wrap o1) (TsBridge_DTS_Wrap o2) = toArray o1 `eq` toArray o2
    where
    toArray = OSet.toUnfoldable :: _ -> Array _

instance Eq a => Monoid (TsBridge_DTS_Wrap (OSet a)) where
  mempty = TsBridge_DTS_Wrap $ OSet.empty

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
