module TsBridge.DTS
  ( TsDeclaration(..)
  , TsFilePath(..)
  , TsImport(..)
  , TsModule(..)
  , TsModuleFile(..)
  , TsName(..)
  , TsProgram(..)
  , TsQualName(..)
  , TsType(..)
  , printTsModule
  , printTsProgram
  ) where

import Prelude

import Data.Array (intersperse)
import Data.Array as Array
import Data.Maybe (Maybe, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String as S
import Data.Tuple.Nested (type (/\), (/\))

-------------------------------------------------------------------------------
-- Types / TsToken
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

  -- Builtins
  | TsTokString
  | TsTokNumber
  | TsTokBoolean

  -- Punctuation
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
  | TsTokFatArrow

  -- Formatting 
  | TsTokWhitespace
  | TsTokNewline

  -- Literals
  | TsTokIdentifier String
  | TsTokStringLiteral String
  | TsTokNumberLiteral Number

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data TsModule = TsModule (Array TsImport) (Array TsDeclaration)

data TsModuleFile = TsModuleFile TsFilePath TsModule

data TsProgram = TsProgram (Array TsModuleFile)

-------------------------------------------------------------------------------
-- Types / TsName
-------------------------------------------------------------------------------

data TsName = TsName String

derive instance Eq TsName
derive instance Ord TsName

-------------------------------------------------------------------------------
-- Types / TsFilePath
-------------------------------------------------------------------------------

data TsFilePath = TsFilePath String

derive instance Eq TsFilePath
derive instance Ord TsFilePath

-------------------------------------------------------------------------------
-- Types / TsDeclaration
-------------------------------------------------------------------------------

data TsDeclaration = TsDeclTypeDef TsName (Array TsName) TsType

derive instance Eq TsDeclaration
derive instance Ord TsDeclaration

-------------------------------------------------------------------------------
-- Types / TsImport
-------------------------------------------------------------------------------

data TsImport = TsImport TsName TsFilePath

derive instance Eq TsImport
derive instance Ord TsImport

-------------------------------------------------------------------------------
-- Types / TsQualName
-------------------------------------------------------------------------------

data TsQualName = TsQualName (Maybe String) String

derive instance Eq TsQualName
derive instance Ord TsQualName

-------------------------------------------------------------------------------
-- Types / TsType
-------------------------------------------------------------------------------

data TsType
  = TsTypeNumber
  | TsTypeString
  | TsTypeBoolean
  | TsTypeArray TsType
  | TsTypeRecord (Array (TsName /\ TsType))
  | TsTypeFunction (Set TsName) (Array (TsName /\ TsType)) TsType
  | TsTypeConstructor TsQualName (Array TsType)

derive instance Eq TsType
derive instance Ord TsType

-------------------------------------------------------------------------------
-- Tokenize
-------------------------------------------------------------------------------

wrapAngles :: Array TsToken -> Array TsToken
wrapAngles x = [ TsTokOpenAngle ] <> x <> [ TsTokCloseAngle ]

wrapBraces :: Array TsToken -> Array TsToken
wrapBraces x = [ TsTokOpenBrace ] <> x <> [ TsTokCloseBrace ]

wrapParens :: Array TsToken -> Array TsToken
wrapParens x = [ TsTokOpenParen ] <> x <> [ TsTokCloseParen ]

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
    TsTypeNumber -> [ TsTokNumber ]
    TsTypeString -> [ TsTokString ]
    TsTypeBoolean -> [ TsTokBoolean ]
    TsTypeArray x -> [ TsTokIdentifier "Array" ] <> wrapAngles (tokenize x)
    TsTypeRecord xs -> wrapBraces
      $ join
      $ intersperse [ TsTokSemicolon ]
      $ (\(k /\ v) -> tokenize k <> [ TsTokColon ] <> tokenize v)
          <$> xs
    TsTypeFunction targs args ret ->
      tokenizeTypeArgs targs
        <> wrapParens (tokenizeFnArg =<< args)
        <> [ TsTokFatArrow ]
        <> tokenize ret
      where
      tokenizeTypeArgs x | Set.isEmpty x = []
      tokenizeTypeArgs x = wrapAngles $ tokenize x

      tokenizeFnArg (k /\ v) = tokenize k <> [ TsTokColon ] <> tokenize v
    TsTypeConstructor qname targs -> tokenize qname <> tokenizeTypeArgs targs
      where
      tokenizeTypeArgs x | Array.length x == 0 = []
      tokenizeTypeArgs xs = wrapAngles $ tokenize xs

instance Tokenize TsDeclaration where
  tokenize = case _ of
    TsDeclTypeDef n _ t ->
      [ TsTokType, TsTokWhitespace ] <> tokenize n <> [ TsTokEquals ] <> tokenize t

instance Tokenize TsModule where
  tokenize (TsModule _ ds) = tokenize ds

instance Tokenize a => Tokenize (Array a) where
  tokenize xs = xs >>= tokenize

instance Tokenize a => Tokenize (Set a) where
  tokenize xs = xs # (Set.toUnfoldable :: _ -> Array _) # tokenize

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

  TsTokNumber -> "number"
  TsTokString -> "string"
  TsTokBoolean -> "boolean"

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
  TsTokFatArrow -> "=>"

  TsTokWhitespace -> " "
  TsTokNewline -> "\n"

  TsTokIdentifier x -> x
  TsTokStringLiteral x -> "\"" <> x <> "\""
  TsTokNumberLiteral x -> show x

printTsModule :: TsModule -> String
printTsModule x = tokenize x <#> printToken # S.joinWith ""

printTsFilePath :: TsFilePath -> String
printTsFilePath (TsFilePath x) = x

printTsModuleFile :: TsModuleFile -> String /\ String
printTsModuleFile (TsModuleFile fp m) = printTsFilePath fp /\ printTsModule m

printTsProgram :: TsProgram -> Array (String /\ String)
printTsProgram (TsProgram xs) = map printTsModuleFile xs
