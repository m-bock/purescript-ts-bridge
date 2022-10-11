module Test.Util where

import Prelude

import Data.Maybe (Maybe(..))
import Dodo as Dodo
import Effect.Aff (Aff)
import Foreign.Object (fromHomogeneous)
import Node.ChildProcess (defaultSpawnOptions)
import PureScript.CST (RecoveredParserResult(..), parseExpr)
import PureScript.CST.Types (Expr)
import Sunde (spawn)
import Test.Spec.Assertions (fail)
import Tidy (defaultFormatOptions, formatExpr)
import Tidy.Doc (FormatDoc(..))

shouldEqual :: forall t. Show t => Eq t => t -> t -> Aff Unit
shouldEqual v1 v2 =
  when (v1 /= v2) do
    r <- spawn
      { cmd: "bash"
      , args:
          [ "-c"
          , "diff --color=always <( printf '%s\n' \"$string2\" ) <( printf '%s\n' \"$string1\" )"
          ]
      , stdin: Nothing
      }
      defaultSpawnOptions
        { env = Just $ fromHomogeneous
            { string1: showPretty v2
            , string2: showPretty v1
            }
        }
    fail ("\n" <> showPretty { actual: v1, expected: v2 } <> "\n\n" <> r.stdout)

printExpr :: Expr Void -> String
printExpr expr =
  formatExpr defaultFormatOptions expr
    # (\(FormatDoc { doc }) -> doc)
    # Dodo.print Dodo.plainText Dodo.twoSpaces

showPretty :: forall a. Show a => a -> String
showPretty = show >>> parseExpr >>> case _ of
  ParseSucceeded m -> printExpr m
  _ -> "<invalid>"
