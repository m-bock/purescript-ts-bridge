module TsBridge.Print
  ( Path(..)
  , TsSource(..)
  , printTsDeclaration
  , printTsDeclarations
  , printTsProgram
  , printTsType
  ) where

import Prelude

import Data.Array (fold, intersperse)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Set.Ordered as OSet
import Data.Tuple.Nested ((/\))
import TsBridge.DTS as DTS

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

newtype TsSource = TsSource String

newtype Path = Path String

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
  | TsTokDeclareModule

  -- Builtins
  | TsTokString
  | TsTokNumber
  | TsTokBoolean
  | TsTokNull

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
  | TsTokAmpersand
  | TsTokPipe

  -- Formatting 
  | TsTokWhitespace
  | TsTokNewline

  -- Literals
  | TsTokIdentifier String
  | TsTokStringLiteral String
  | TsTokNumberLiteral Number
  | TsTokLineComment String

-------------------------------------------------------------------------------
-- Tokenize
-------------------------------------------------------------------------------

class Tokenize a where
  tokenize :: a -> TsTokens

instance Tokenize DTS.TsName where
  tokenize x = [ TsTokIdentifier $ DTS.printTsName x ]

instance Tokenize DTS.TsQualName where
  tokenize (DTS.TsQualName s x) =
    maybe []
      ( \(DTS.TsImportPath n) ->
          [ TsTokImport
          , TsTokOpenParen
          , TsTokStringLiteral n
          , TsTokCloseParen
          , TsTokDot
          ]
      )
      s
      <> tokenize x

instance Tokenize DTS.TsType where
  tokenize = case _ of
    DTS.TsTypeNumber ->
      [ TsTokNumber ]

    DTS.TsTypeString ->
      [ TsTokString ]

    DTS.TsTypeBoolean ->
      [ TsTokBoolean ]

    DTS.TsTypeNull ->
      [ TsTokNull ]

    DTS.TsTypeArray x ->
      [ TsTokIdentifier "Array" ] <> wrapAngles (tokenize x)

    DTS.TsTypeReadonlyArray x ->
      [ TsTokIdentifier "ReadonlyArray" ] <> wrapAngles (tokenize x)

    DTS.TsTypeIntersection xs ->
      join $ intersperse [ TsTokAmpersand ] $ (wrapParens <<< tokenize) <$> xs

    DTS.TsTypeUnion xs ->
      join $ intersperse [ TsTokWhitespace, TsTokPipe, TsTokWhitespace ] $ (wrapParens <<< tokenize) <$> xs

    DTS.TsTypeRecord xs ->
      wrapBraces $
        applyWhenNotEmpty (\xs' -> [ TsTokWhitespace ] <> (tokenize =<< xs')) xs

    DTS.TsTypeFunction targsQ args ret ->
      tokenize targsQ
        <> wrapParens (tokenize =<< args)
        <> [ TsTokWhitespace, TsTokFatArrow, TsTokWhitespace ]
        <> tokenize ret

    DTS.TsTypeConstructor qname targs ->
      tokenize qname <> tokenize targs

    DTS.TsTypeUniqueSymbol ->
      [ TsTokUnique, TsTokWhitespace, TsTokSymbol ]

    DTS.TsTypeVar n ->
      tokenize n

    DTS.TsTypeVoid -> [ TsTokVoid ]

    DTS.TsTypeTypelevelString str -> [ TsTokStringLiteral str ]

instance Tokenize DTS.TsFnArg where
  tokenize (DTS.TsFnArg k v) = tokenize k
    <> [ TsTokColon, TsTokWhitespace ]
    <> tokenize v

instance Tokenize DTS.TsTypeArgs where
  tokenize (DTS.TsTypeArgs xs) | Array.length xs == 0 = []
  tokenize (DTS.TsTypeArgs xs) = wrapAngles $ sepByComma $ tokenize <$> xs

instance Tokenize DTS.TsTypeArgsQuant where
  tokenize (DTS.TsTypeArgsQuant xs) | OSet.isEmpty $ unwrap xs = []
  tokenize (DTS.TsTypeArgsQuant xs) = wrapAngles
    $ sepByComma
    $ tokenize
        <$> (OSet.toUnfoldable $ unwrap xs)

instance Tokenize DTS.TsRecordField where
  tokenize (DTS.TsRecordField k { readonly, optional } v) =
    (if readonly then [ TsTokReadonly, TsTokWhitespace ] else [])
      <> [ TsTokStringLiteral k ]
      <> (if optional then [ TsTokWhitespace ] else [])
      <> [ TsTokColon, TsTokWhitespace ]
      <> tokenize v
      <> [ TsTokSemicolon, TsTokWhitespace ]

instance Tokenize DTS.TsDeclVisibility where
  tokenize DTS.Private = []
  tokenize DTS.Public = [ TsTokExport, TsTokWhitespace ]

instance Tokenize DTS.TsDeclaration where
  tokenize = case _ of
    DTS.TsDeclTypeDef n vis targs t ->
      tokenize vis
        <> [ TsTokType, TsTokWhitespace ]
        <> tokenize n
        <>
          ( applyWhenNotEmpty (wrapAngles <<< sepByComma)
              $ tokenize <$> (OSet.toUnfoldable $ unwrap targs)
          )
        <> [ TsTokWhitespace, TsTokEquals, TsTokWhitespace ]
        <> tokenize t

    DTS.TsDeclValueDef n vis t ->
      tokenize vis
        <> [ TsTokConst, TsTokWhitespace ]
        <> tokenize n
        <> [ TsTokWhitespace, TsTokColon, TsTokWhitespace ]
        <> tokenize t

    DTS.TsDeclComments xs ->
      xs >>= \x -> [ TsTokLineComment x ]

instance Tokenize DTS.TsModule where
  tokenize (DTS.TsModule ds) =
    (ds <#> tokenize # sepByDoubleNewline)

instance Tokenize DTS.TsModulePath where
  tokenize (DTS.TsModulePath x) =
    [ TsTokStringLiteral x ]

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

applyWhenNotEmpty :: forall a b. (Array a -> Array b) -> Array a -> Array b
applyWhenNotEmpty _ xs | Array.null xs = []
applyWhenNotEmpty f xs = f xs

-------------------------------------------------------------------------------
-- Print
-------------------------------------------------------------------------------

printToken :: TsToken -> TsSource
printToken tsToken = TsSource case tsToken of
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
  TsTokDeclareModule ->
    "declare module"

  TsTokNumber ->
    "number"
  TsTokString ->
    "string"
  TsTokBoolean ->
    "boolean"
  TsTokNull ->
    "null"

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
  TsTokAmpersand ->
    "&"
  TsTokPipe ->
    "|"

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
  TsTokLineComment x ->
    "// " <> x <> "\n"

printTsModule :: DTS.TsModule -> TsSource
printTsModule x = tokenize x <#> printToken # fold

printTsPath :: DTS.TsFilePath -> Path
printTsPath (DTS.TsFilePath x) = Path x

printTsType :: DTS.TsType -> TsSource
printTsType x = tokenize x <#> printToken # fold

printTsDeclaration :: DTS.TsDeclaration -> TsSource
printTsDeclaration x = x # tokenize # map printToken >>> fold

printTsDeclarations :: Array DTS.TsDeclaration -> Array TsSource
printTsDeclarations x = x <#> tokenize <#> map printToken >>> fold

printTsProgram :: DTS.TsProgram -> Map Path TsSource
printTsProgram (DTS.TsProgram xs) = xs
  # (Map.toUnfoldable :: _ -> Array _)
  <#> (\(k /\ v) -> printTsPath k /\ printTsModule v)
  # Map.fromFoldable

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

derive newtype instance Semigroup TsSource

derive newtype instance Semigroup Path

derive newtype instance Eq TsSource

derive newtype instance Eq Path

derive newtype instance Ord Path

derive newtype instance Show TsSource

derive newtype instance Show Path

derive newtype instance Monoid TsSource

derive newtype instance Monoid Path

derive instance Newtype TsSource _

derive instance Newtype Path _