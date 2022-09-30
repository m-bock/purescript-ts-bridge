module TsBridge.Print
  ( printTsDeclarations
  , printTsName
  , printTsProgram
  , printTsType
  ) where

import Prelude

import Data.Array (intersperse)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.Set.Ordered as OSet
import Data.String as S
import Data.Tuple.Nested (type (/\), (/\))
import TsBridge.DTS (TsDeclVisibility(..), TsDeclaration(..), TsFilePath(..), TsFnArg(..), TsImport(..), TsModule(..), TsModuleAlias(..), TsModuleFile(..), TsModulePath(..), TsName(..), TsProgram(..), TsQualName(..), TsRecordField(..), TsType(..), TsTypeArgs(..), TsTypeArgsQuant(..))

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
  | TsTokSymbol

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
  | TsTokQuestionMark

  -- Formatting 
  | TsTokWhitespace
  | TsTokNewline

  -- Literals
  | TsTokIdentifier String
  | TsTokStringLiteral String
  | TsTokNumberLiteral Number

-------------------------------------------------------------------------------
-- Tokenize
-------------------------------------------------------------------------------

wrap :: TsTokens -> TsTokens -> TsTokens -> TsTokens
wrap p q x = p <> x <> q

sepBy :: TsTokens -> Array TsTokens -> TsTokens
sepBy sep xs = intersperse sep xs # join

postfix :: TsTokens -> Array TsTokens -> TsTokens
postfix tok xs = xs <#> (_ <> tok) # join

wrapAngles :: TsTokens -> TsTokens
wrapAngles = wrap [ TsTokOpenAngle ] [ TsTokCloseAngle ]

wrapBraces :: TsTokens -> TsTokens
wrapBraces = wrap [ TsTokOpenBrace ] [ TsTokCloseBrace ]

wrapParens :: TsTokens -> TsTokens
wrapParens = wrap [ TsTokOpenParen ] [ TsTokCloseParen ]

sepByComma :: Array TsTokens -> TsTokens
sepByComma = sepBy [ TsTokComma, TsTokWhitespace ]

sepByDoubleNewline :: Array TsTokens -> TsTokens
sepByDoubleNewline = sepBy [ TsTokNewline, TsTokNewline ]

-- postfixSemicolon :: Array TsTokens -> TsTokens
-- postfixSemicolon = postfix [ TsTokSemicolon ]

-- postfixDoubleNewline :: Array TsTokens -> TsTokens
-- postfixDoubleNewline = postfix [ TsTokNewline, TsTokNewline ]

postfixNewline :: Array TsTokens -> TsTokens
postfixNewline = postfix [ TsTokNewline ]

-- sectionNewline :: (Array TsTokens -> TsTokens) -> Array TsTokens -> TsTokens
-- sectionNewline f xs | Array.length xs == 0 = []
-- sectionNewline f xs = f xs <> [ TsTokNewline ]

applyWhenNotEmpty :: forall a b. (Array a -> Array b) -> Array a -> Array b
applyWhenNotEmpty _ xs | Array.null xs = []
applyWhenNotEmpty f xs = f xs

class Tokenize a where
  tokenize :: a -> TsTokens

instance Tokenize TsName where
  tokenize (TsName x) = [ TsTokIdentifier x ]

instance Tokenize TsModuleAlias where
  tokenize (TsModuleAlias x) = [ TsTokIdentifier x ]

instance Tokenize TsQualName where
  tokenize (TsQualName s x) =
    maybe [] (\n -> tokenize n <> [ TsTokDot ]) s
      <> tokenize x

instance Tokenize TsType where
  tokenize = case _ of
    TsTypeNumber ->
      [ TsTokNumber ]

    TsTypeString ->
      [ TsTokString ]

    TsTypeBoolean ->
      [ TsTokBoolean ]

    TsTypeArray x ->
      [ TsTokIdentifier "Array" ] <> wrapAngles (tokenize x)

    TsTypeRecord xs ->
      wrapBraces $
        applyWhenNotEmpty (\xs' -> [ TsTokWhitespace ] <> (tokenize =<< xs')) xs

    TsTypeFunction targsQ args ret ->
      tokenize targsQ
        <> wrapParens (tokenize =<< args)
        <> [ TsTokWhitespace, TsTokFatArrow, TsTokWhitespace ]
        <> tokenize ret

    TsTypeConstructor qname targs ->
      tokenize qname <> tokenize targs

    TsTypeUniqueSymbol ->
      [ TsTokUnique, TsTokWhitespace, TsTokSymbol ]

    TsTypeVar n ->
      tokenize n

instance Tokenize TsFnArg where
  tokenize (TsFnArg k v) = tokenize k
    <> [ TsTokColon, TsTokWhitespace ]
    <> tokenize v

instance Tokenize TsTypeArgs where
  tokenize (TsTypeArgs xs) | Array.length xs == 0 = []
  tokenize (TsTypeArgs xs) = wrapAngles $ sepByComma $ tokenize <$> xs

instance Tokenize TsTypeArgsQuant where
  tokenize (TsTypeArgsQuant xs) | OSet.isEmpty $ unwrap xs = []
  tokenize (TsTypeArgsQuant xs) = wrapAngles
    $ sepByComma
    $ tokenize
        <$> (OSet.toUnfoldable $ unwrap xs)

instance Tokenize TsRecordField where
  tokenize (TsRecordField k { readonly, optional } v) =
    (if readonly then [ TsTokReadonly, TsTokWhitespace ] else [])
      <> tokenize k
      <> (if optional then [ TsTokWhitespace ] else [])
      <> [ TsTokColon, TsTokWhitespace ]
      <> tokenize v
      <> [ TsTokSemicolon, TsTokWhitespace ]

instance Tokenize TsDeclVisibility where
  tokenize Private = []
  tokenize Public = [ TsTokExport, TsTokWhitespace ]

instance Tokenize TsDeclaration where
  tokenize = case _ of
    TsDeclTypeDef n vis targs t ->
      tokenize vis
        <> [ TsTokType, TsTokWhitespace ]
        <> tokenize n
        <>
          ( applyWhenNotEmpty (wrapAngles <<< sepByComma)
              $ tokenize <$> (OSet.toUnfoldable $ unwrap targs)
          )
        <> [ TsTokWhitespace, TsTokEquals, TsTokWhitespace ]
        <> tokenize t

    TsDeclValueDef n vis t ->
      tokenize vis
        <> [ TsTokConst, TsTokWhitespace ]
        <> tokenize n
        <> [ TsTokWhitespace, TsTokColon, TsTokWhitespace ]
        <> tokenize t

instance Tokenize TsModule where
  tokenize (TsModule is ds) =
    ( is
        # Set.toUnfoldable
        <#> tokenize
        # applyWhenNotEmpty (postfixNewline >>> (_ <> [ TsTokNewline ]))
    )
      <> (ds <#> tokenize # sepByDoubleNewline)

instance Tokenize TsImport where
  tokenize (TsImport n fp) =
    [ TsTokImport, TsTokWhitespace, TsTokAsterisk, TsTokWhitespace, TsTokAs, TsTokWhitespace ]
      <> tokenize n
      <> [ TsTokWhitespace, TsTokFrom, TsTokWhitespace ]
      <> tokenize fp

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
  TsTokSymbol ->
    "symbol"

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
  TsTokQuestionMark ->
    "?"

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

printTsName :: TsName -> String
printTsName x = tokenize x <#> printToken # S.joinWith ""

printTsModule :: TsModule -> String
printTsModule x = tokenize x <#> printToken # S.joinWith ""

printTsType :: TsType -> String
printTsType x = tokenize x <#> printToken # S.joinWith ""

printTsDeclarations :: Array TsDeclaration -> Array String
printTsDeclarations x = x <#> tokenize <#> map printToken >>> S.joinWith ""

printTsFilePath :: TsFilePath -> String
printTsFilePath (TsFilePath x y) = x <> "." <> y

-- printTsModuleFile :: TsModuleFile -> String /\ String
-- printTsModuleFile (TsModuleFile fp m) = printTsFilePath fp /\ printTsModule m

printTsProgram :: TsProgram -> Map String String
printTsProgram (TsProgram xs) = xs
  # (Map.toUnfoldable :: _ -> Array _)
  <#> (\(k /\ v) -> printTsFilePath k /\ printTsModule v)
  # Map.fromFoldable