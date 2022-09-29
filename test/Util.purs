module Test.Util where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Foldable (foldMap)
import Effect.Aff (Error)
import PureScript.CST (RecoveredParserResult(..), parseExpr)
import PureScript.CST.Print as Print
import PureScript.CST.Range (class TokensOf, tokensOf)
import PureScript.CST.Range.TokenList as TokenList
import PureScript.CST.Types (Expr)
import Test.Spec.Assertions (fail)

shouldEqual :: forall m t. MonadThrow Error m => Show t => Eq t => t -> t -> m Unit
shouldEqual v1 v2 = when (v1 /= v2)
  $ fail
  $ "\n"
      <> showPretty v1
      <> "\n\n≠\n\n"
      <> showPretty v2
      <> "\n"

printExpr :: forall e. TokensOf e => Expr e -> String
printExpr mod =
  foldMap Print.printSourceToken (TokenList.toArray (tokensOf mod))

showPretty :: forall a. Show a => a -> String
showPretty = show >>> parseExpr >>> case _ of
  ParseSucceeded m -> printExpr m
  _ -> "<invalid>"