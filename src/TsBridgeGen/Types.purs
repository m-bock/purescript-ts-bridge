module TsBridgeGen.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import TypedEnv as TypedEnv

data TsBridgeGenError
  = ErrSpawn String (Array String)
  | ErrParseModule
  | ErrReadFile String
  | ErrExpandGlobs
  | ErrParseEnvVars TypedEnv.EnvError

newtype Glob = Glob String

newtype ModuleName = ModuleName String

newtype Name = Name String

data PursModule = PursModule ModuleName (Array PursDef)

data Import
  = ImportAuto { from :: ModuleName, as :: Name }
  | ImportUser String

data PursDef
  = DefData Name
  | DefNewtype Name
  | DefType Name
  | DefValue Name

derive instance Generic PursDef _
derive instance Generic Name _
derive instance Generic PursModule _
derive instance Generic ModuleName _

derive instance Eq PursModule
derive instance Eq ModuleName
derive instance Eq PursDef
derive instance Eq Name
derive instance Eq Import

derive instance Ord ModuleName
derive instance Ord Name
derive instance Ord Import

instance Show PursDef where
  show = genericShow

instance Show Name where
  show = genericShow

instance Show PursModule where
  show = genericShow

instance Show ModuleName where
  show = genericShow

derive instance Generic TsBridgeGenError _

derive instance Eq TsBridgeGenError

instance Show TsBridgeGenError where
  show = genericShow
