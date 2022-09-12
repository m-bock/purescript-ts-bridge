module TsBridge.DTS
  ( TsDeclaration(..)
  , TsFilePath(..)
  , TsFnArg(..)
  , TsImport(..)
  , TsModule(..)
  , TsModuleFile(..)
  , TsModulePath(..)
  , TsName(..)
  , TsProgram(..)
  , TsQualName(..)
  , TsRecord(..)
  , TsRecordField(..)
  , TsType(..)
  , TsTypeArgs(..)
  , TsTypeArgsQuant(..)
  , printTsModule
  , printTsProgram
  ) where

import Prelude

import Data.Array (intersperse)
import Data.Array as Array
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String as S
import Data.Tuple.Nested (type (/\), (/\))

-------------------------------------------------------------------------------
-- Types / TsToken
-------------------------------------------------------------------------------

type TsTokens = Array TsToken

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

data TsDeclaration = TsDeclTypeDef TsName (Array TsName) TsType

data TsType
  = TsTypeNumber
  | TsTypeString
  | TsTypeBoolean
  | TsTypeArray TsType
  | TsTypeRecord TsRecord
  | TsTypeFunction TsTypeArgsQuant (Array TsFnArg) TsType
  | TsTypeConstructor TsQualName TsTypeArgs

data TsModule = TsModule (Set TsImport) (Array TsDeclaration)

data TsModuleFile = TsModuleFile TsFilePath TsModule

data TsProgram = TsProgram (Map TsFilePath TsModule)

newtype Statements a = Statements a

data TsName = TsName String

data TsModulePath = TsModulePath String

data TsFilePath = TsFilePath String

data TsImport = TsImport TsName TsModulePath

data TsQualName = TsQualName (Maybe String) String

newtype TsTypeArgsQuant = TsTypeArgsQuant (Set TsName)

newtype TsTypeArgs = TsTypeArgs (Array TsType)

data TsFnArg = TsFnArg TsName TsType

newtype TsRecord = TsRecord (Array TsRecordField)

data TsRecordField = TsRecordField TsName TsType

-------------------------------------------------------------------------------
-- Class / Eq
-------------------------------------------------------------------------------

derive instance Eq TsRecordField
derive instance Eq TsFnArg
derive instance Eq TsRecord
derive instance Eq TsTypeArgs
derive instance Eq TsImport
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
derive instance Ord TsRecord
derive instance Ord TsTypeArgs
derive instance Ord TsImport
derive instance Ord TsQualName
derive instance Ord TsTypeArgsQuant
derive instance Ord TsDeclaration
derive instance Ord TsType
derive instance Ord TsName
derive instance Ord TsModulePath
derive instance Ord TsFilePath

-------------------------------------------------------------------------------
-- Tokenize
-------------------------------------------------------------------------------

wrap :: TsTokens -> TsTokens -> TsTokens -> TsTokens
wrap p q x = p  <> x <>  q 

sepBy :: TsTokens -> Array TsTokens -> TsTokens
sepBy sep xs = intersperse sep xs # join


postfix :: TsTokens -> Array TsTokens -> TsTokens
postfix tok xs = xs <#> (_ <> tok) # join

wrapAngles :: TsTokens -> TsTokens
wrapAngles = wrap [TsTokOpenAngle] [TsTokCloseAngle]

wrapBraces :: TsTokens -> TsTokens
wrapBraces = wrap [TsTokOpenBrace] [TsTokCloseBrace]

wrapParens :: TsTokens -> TsTokens
wrapParens = wrap [TsTokOpenParen] [TsTokCloseParen]

sepByComma :: Array TsTokens -> TsTokens
sepByComma = sepBy [ TsTokComma ]

sepByDoubleNewline :: Array TsTokens -> TsTokens
sepByDoubleNewline = sepBy [ TsTokNewline, TsTokNewline ]

postfixSemicolon :: Array TsTokens -> TsTokens
postfixSemicolon = postfix [ TsTokSemicolon ]

postfixDoubleNewline :: Array TsTokens -> TsTokens
postfixDoubleNewline = postfix [ TsTokNewline, TsTokNewline ]

sectionNewline :: (Array TsTokens -> TsTokens) -> Array TsTokens -> TsTokens
sectionNewline f xs | Array.length xs == 0 = []
sectionNewline f xs = f xs <> [ TsTokNewline ]

applyWhenNotEmpty :: forall a b. (Array a -> Array b) -> Array a -> Array b
applyWhenNotEmpty f xs | Array.null xs = []
applyWhenNotEmpty f xs = f xs



class Tokenize a where
  tokenize :: a -> TsTokens

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
    TsTypeRecord r -> tokenize r
    TsTypeFunction targsQ args ret -> tokenize targsQ
      <> wrapParens (tokenize =<< args)
      <> [ TsTokWhitespace, TsTokFatArrow, TsTokWhitespace ]
      <> tokenize ret
    TsTypeConstructor qname targs -> tokenize qname <> tokenize targs

instance Tokenize TsFnArg where
  tokenize (TsFnArg k v) = tokenize k
    <> [ TsTokColon, TsTokWhitespace ]
    <> tokenize v

instance Tokenize TsTypeArgs where
  tokenize (TsTypeArgs xs) | Array.length xs == 0 = []
  tokenize (TsTypeArgs xs) = wrapAngles $ sepByComma $ tokenize <$> xs

instance Tokenize TsTypeArgsQuant where
  tokenize (TsTypeArgsQuant xs) | Set.isEmpty xs = []
  tokenize (TsTypeArgsQuant xs) = wrapAngles
    $ sepByComma
    $ tokenize
        <$> Set.toUnfoldable xs

instance Tokenize TsRecord where
  tokenize (TsRecord []) = wrapBraces []
  tokenize (TsRecord xs) =
    wrapBraces $
      [ TsTokWhitespace ] <> (join $ tokenize <$> xs)

instance Tokenize TsRecordField where
  tokenize (TsRecordField k v) =
    tokenize k
      <> [ TsTokColon, TsTokWhitespace ]
      <> tokenize v
      <> [ TsTokSemicolon, TsTokWhitespace ]

instance Tokenize TsDeclaration where
  tokenize = case _ of
    TsDeclTypeDef n _ t ->
      [ TsTokType, TsTokWhitespace ]
        <> tokenize n
        <> [ TsTokWhitespace, TsTokEquals, TsTokWhitespace ]
        <> tokenize t

instance Tokenize TsModule where
  tokenize (TsModule is ds) =
    (is # Set.toUnfoldable <#> tokenize # sectionNewline sepByDoubleNewline)
      <> (ds <#> tokenize # sepByDoubleNewline)

instance Tokenize TsImport where
  tokenize (TsImport n fp) =
    [ TsTokImport, TsTokWhitespace, TsTokAsterisk, TsTokWhitespace, TsTokAs, TsTokWhitespace ]
      <> tokenize n
      <> [ TsTokFrom ]
      <> tokenize fp

instance Tokenize TsFilePath where
  tokenize (TsFilePath x) =
    [ TsTokStringLiteral x ]

instance Tokenize TsModulePath where
  tokenize (TsModulePath x) =
    [ TsTokStringLiteral x ]

-------------------------------------------------------------------------------
-- Print
-------------------------------------------------------------------------------

printToken :: TsToken -> String
printToken = case _ of
  TsTokConst ->
    "const"
  TsTokDefault ->
    "default"
  TsTokExport ->
    "export"
  TsTokDeclare ->
    "declare"
  TsTokImport ->
    "import"
  TsTokVoid ->
    "void"
  TsTokReadonly ->
    "readonly"
  TsTokUnique ->
    "unique"
  TsTokAs ->
    "as"
  TsTokFrom ->
    "from"
  TsTokType ->
    "type"

  TsTokNumber ->
    "number"
  TsTokString ->
    "string"
  TsTokBoolean ->
    "boolean"

  TsTokSemicolon ->
    ";"
  TsTokAsterisk ->
    "*"
  TsTokOpenParen ->
    "("
  TsTokCloseParen ->
    ")"
  TsTokOpenBracket ->
    "["
  TsTokCloseBracket ->
    "]"
  TsTokOpenBrace ->
    "{"
  TsTokCloseBrace ->
    "}"
  TsTokOpenAngle ->
    "<"
  TsTokCloseAngle ->
    ">"
  TsTokComma ->
    ","
  TsTokEquals ->
    "="
  TsTokColon ->
    ":"
  TsTokDot ->
    "."
  TsTokFatArrow ->
    "=>"

  TsTokWhitespace ->
    " "
  TsTokNewline ->
    "\n"

  TsTokIdentifier x ->
    x
  TsTokStringLiteral x ->
    "'" <> x <> "'"
  TsTokNumberLiteral x ->
    show x

printTsModule :: TsModule -> String
printTsModule x = tokenize x <#> printToken # S.joinWith ""

printTsFilePath :: TsFilePath -> String
printTsFilePath (TsFilePath x) = x

printTsModuleFile :: TsModuleFile -> String /\ String
printTsModuleFile (TsModuleFile fp m) = printTsFilePath fp /\ printTsModule m

printTsProgram :: TsProgram -> Map String String
printTsProgram (TsProgram xs) = xs
  # (Map.toUnfoldable :: _ -> Array _)
  <#> (\(k /\ v) -> printTsFilePath k /\ printTsModule v)
  # Map.fromFoldable
