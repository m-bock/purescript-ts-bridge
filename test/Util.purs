module Test.Util where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Foldable (foldMap)
import Dodo as Dodo
import Effect.Aff (Error)
import PureScript.CST (RecoveredParserResult(..), parseExpr)
import PureScript.CST.Print (TokenOption(..))
import PureScript.CST.Print as Print
import PureScript.CST.Range (class TokensOf, tokensOf)
import PureScript.CST.Range.TokenList as TokenList
import PureScript.CST.Types (Expr)
import Test.Spec.Assertions (fail)
import Tidy (FormatDoc, defaultFormatOptions, formatExpr)
import Tidy.Doc (FormatDoc(..))

shouldEqual :: forall m t. MonadThrow Error m => Show t => Eq t => t -> t -> m Unit
shouldEqual v1 v2 = when (v1 /= v2)
  $ fail
  $ "\n"
      <> showPretty v2
      <> "\n\n≠\n\n"
      <> showPretty v1
      <> "\n"

printExpr :: Expr Void -> String
printExpr expr =
  formatExpr defaultFormatOptions expr
    # (\(FormatDoc { doc }) -> doc)
    # Dodo.print Dodo.plainText Dodo.twoSpaces

showPretty :: forall a. Show a => a -> String
showPretty = show >>> parseExpr >>> case _ of
  ParseSucceeded m -> printExpr m
  _ -> "<invalid>"
