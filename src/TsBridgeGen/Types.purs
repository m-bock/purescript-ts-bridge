module TsBridgeGen.Types where

import Prelude

import Data.Argonaut (class DecodeJson, JsonDecodeError)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Typelevel.Undefined (undefined)
import Node.Path (FilePath)
import Prim.TypeError (class Warn, Text)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Arbitrary (genericArbitrary)
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
  | AtFileSection
      { path :: FilePath
      , section :: FileSection
      , filePos :: Maybe SourcePosition
      , localPos :: SourcePosition
      }
      AppError

newtype FileSection = FileSection String

derive instance Newtype FileSection _

data AppLog = LogLiteral String

newtype SourcePosition = SourcePosition { line :: Int, column :: Int }

instance Monoid SourcePosition where
  mempty = SourcePosition { line: 0, column: 0 }

instance Semigroup SourcePosition where
  append (SourcePosition sp1) (SourcePosition sp2) | sp2.line == 0 =
    SourcePosition
      { line: sp1.line
      , column: sp1.column + sp2.column
      }
  append (SourcePosition sp1) (SourcePosition sp2) =
    SourcePosition
      { line: sp1.line + sp2.line
      , column: sp2.column
      }

instance Arbitrary SourcePosition where
  arbitrary = genericArbitrary

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
  = UnexpectedTokenAtPos String Int
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
derive instance Generic FileSection _

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

instance Show FileSection where
  show = genericShow

derive instance Generic AppError _

derive instance Eq SourcePosition

derive instance Eq AppError

derive instance Eq FileSection

derive instance Eq ErrorParseToJson

instance Show AppError where
  show x = genericShow x

instance Show ErrorParseToJson where
  show = genericShow

instance Show SourcePosition where
  show = genericShow

derive newtype instance DecodeJson Glob

derive newtype instance DecodeJson ModuleGlob
