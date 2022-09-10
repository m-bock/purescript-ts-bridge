module TsBridge.DTS where

import Prelude

import Data.Array (intercalate, intersperse)
import Data.Maybe (Maybe, maybe)
import Data.String as S
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Undefined (undefined)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data TsToken
  -- Keywords
  = TsTokConst
  | TsTokDefault
  | TsTokExport
  | TsTokDeclare
  | TsTokImport
  | TsTokVoid
  | TsTokReadonly
  | TsTokUnique
  | TsTokAs
  | TsTokFrom
  | TsTokType
  -- Puntuation
  | TsTokSemicolon
  | TsTokAsterisk
  | TsTokOpenParen
  | TsTokCloseParen
  | TsTokOpenBracket
  | TsTokCloseBracket
  | TsTokOpenBrace
  | TsTokCloseBrace
  | TsTokOpenAngle
  | TsTokCloseAngle
  | TsTokComma
  | TsTokEquals
  | TsTokColon
  | TsTokDot
  --
  | TsTokWhitespace
  | TsTokNewline
  -- 
  | TsTokIdentifier String
  | TsTokStringLiteral String
  | TsTokNumberLiteral Number
  | TsTokSingleComment String
  | TsTokMultiComment String

data TsName = TsName String

data TsFilePath = TsFilePath String

data TsDeclaration = TsDeclTypeDef TsName (Array TsName) TsType

data TsImport

data TsQualName = TsQualName (Maybe String) String

data TsType = -- TsTypeAny
  --   | TsTypeNull
  --   | TsTypeNever
  --   | TsTypeUndefined
  --   | TsTypeString
  TsTypeNumber

--   | TsTypeBoolean
--   | TsTypeArray Type
--   | TsTypeRecord (Array (TsName /\ Type))
--   | TsTypeFunction (Set TsName) (Array (TsName /\ Type)) Type
--   | TsTypeVar TsName
--   | TsTypeConstructor TsQualName (Array Type)
--   | TsTypeOpaque TsQualName (Array TsName)
--   | TsTypeUnion Type Type
--   | TsTypeTLString String
--   | TsTypeUniqueSymbol
--   | TsTypeIsPred TsName Type

data TsModule = TsModule (Array TsImport) (Array TsDeclaration)

data TsModuleFile = TsModuleFile TsFilePath TsModule

data TsProgram = TsProgram (Array TsModuleFile)

-------------------------------------------------------------------------------
-- Tokenize
-------------------------------------------------------------------------------

class Tokenize a where
  tokenize :: a -> Array TsToken

instance Tokenize TsName where
  tokenize (TsName x) = [ TsTokIdentifier x ]

instance Tokenize TsQualName where
  tokenize (TsQualName s x) =
    maybe [] (\n -> [ TsTokIdentifier n, TsTokDot ]) s
      <> [ TsTokIdentifier x ]

instance Tokenize TsType where
  tokenize = case _ of
    TsTypeNumber -> [ TsTokIdentifier "number" ]

instance Tokenize TsDeclaration where
  tokenize = case _ of
    TsDeclTypeDef n _ t -> intersperse TsTokWhitespace $
      [ TsTokType ] <> tokenize n <> [ TsTokEquals ] <> tokenize t

instance Tokenize TsModule where
  tokenize (TsModule _ ds) = tokenize ds

instance Tokenize a => Tokenize (Array a) where
  tokenize xs = xs >>= tokenize

-------------------------------------------------------------------------------
-- Print
-------------------------------------------------------------------------------

printToken :: TsToken -> String
printToken = case _ of
  TsTokConst -> "const"
  TsTokDefault -> "default"
  TsTokExport -> "export"
  TsTokDeclare -> "declare"
  TsTokImport -> "import"
  TsTokVoid -> "void"
  TsTokReadonly -> "readonly"
  TsTokUnique -> "unique"
  TsTokAs -> "as"
  TsTokFrom -> "from"
  TsTokType -> "type"
  -- Puntuation
  TsTokSemicolon -> ";"
  TsTokAsterisk -> "*"
  TsTokOpenParen -> "("
  TsTokCloseParen -> ")"
  TsTokOpenBracket -> "["
  TsTokCloseBracket -> "]"
  TsTokOpenBrace -> "{"
  TsTokCloseBrace -> "}"
  TsTokOpenAngle -> "<"
  TsTokCloseAngle -> ">"
  TsTokComma -> ","
  TsTokEquals -> "="
  TsTokColon -> ":"
  TsTokDot -> "."
  --
  TsTokWhitespace -> " "
  TsTokNewline -> "\n"
  -- 
  TsTokIdentifier x -> x
  TsTokStringLiteral x -> "\"" <> x <> "\""
  TsTokNumberLiteral x -> show x
  TsTokSingleComment x -> "TODO"
  TsTokMultiComment x -> "TODO"

printTsDeclaration :: TsDeclaration -> String
printTsDeclaration = undefined

printTsDeclarations :: Array TsDeclaration -> String
printTsDeclarations = undefined

printTsModule :: TsModule -> String
printTsModule x = tokenize x <#> printToken # S.joinWith ""

printTsFilePath :: TsFilePath -> String
printTsFilePath (TsFilePath x) = x

printTsModuleFile :: TsModuleFile -> String /\ String
printTsModuleFile (TsModuleFile fp m) = printTsFilePath fp /\ printTsModule m

printTsProgram :: TsProgram -> Array (String /\ String)
printTsProgram (TsProgram xs) = map printTsModuleFile xs
