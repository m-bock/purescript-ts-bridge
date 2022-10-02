module TsBridgeGen.Types where

import Prelude

import Data.Argonaut (class DecodeJson, JsonDecodeError)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Node.Path (FilePath)
import TypedEnv as TypedEnv

data AppError
  = ErrSpawn String (Array String)
  | ErrParseModule
  | ErrReadFile String
  | ErrWriteFile String
  | ErrExpandGlobs
  | ErrParseEnvVars TypedEnv.EnvError
  | ErrLiteral String
  | ErrParseToJson ErrorParseToJson
  | ErrParseToData JsonDecodeError
  | ErrUnknown
  | AtFilePosition FilePath SourcePosition AppError

data AppLog
  = LogLiteral String
  | LogError AppError

newtype SourcePosition = SourcePosition { line :: Int, column :: Int }

data AppWarning = WarnLiteral String

newtype Glob = Glob String

newtype ModuleGlob = ModuleGlob String

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
  | DefUnsupportedInstAndExport Name String
  | DefUnsupportedExport Name String

data ErrorParseToJson
  = UnexpectedTokenAtPos String SourcePosition
  | UnexpectedEndOfInput
  | Other String

derive instance Generic PursDef _
derive instance Generic Name _
derive instance Generic PursModule _
derive instance Generic ModuleName _
derive instance Generic AppLog _
derive instance Generic AppWarning _
derive instance Generic ErrorParseToJson _
derive instance Generic SourcePosition _

derive instance Eq PursModule
derive instance Eq ModuleName
derive instance Eq PursDef
derive instance Eq Name
derive instance Eq Import
derive instance Eq AppLog

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

instance Show AppLog where
  show = genericShow

instance Show AppWarning where
  show = genericShow

derive instance Generic AppError _

derive instance Eq SourcePosition

derive instance Eq AppError

derive instance Eq ErrorParseToJson

instance Show AppError where
  show x = genericShow x

instance Show ErrorParseToJson where
  show = genericShow

instance Show SourcePosition where
  show = genericShow

derive newtype instance DecodeJson Glob

derive newtype instance DecodeJson ModuleGlob
