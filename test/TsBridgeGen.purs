module Test.TsBridgeGen where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as Str
import Data.Tuple (fst)
import PureScript.CST (RecoveredParserResult(..), parseDecl)
import Test.Spec (Spec, describe, it)
import Test.Util (shouldEqual)
import TsBridgeGen (ModuleName(..), Name(..), PursDef(..), PursModule(..), genInstances, getPursDef, printDecls, runImportWriterM)

recResToMaybe :: forall f. RecoveredParserResult f -> Maybe (f Void)
recResToMaybe = case _ of
  ParseSucceeded x -> Just x
  _ -> Nothing

spec :: Spec Unit
spec = do
  --   describe "" do
  --     let
  --       x = Str.joinWith "\n"
  --         [ "module My where"
  --         , "data Foo = Bar | Baz"
  --         ]
  --     it "" do
  --       x
  --         # parseCstModule
  --         <#> getPursModule
  --         # shouldEqual (Right $ PursModule (ModuleName "my") [ DefData (Name "Foo") ])

  describe "" do
    it "" do
      "data Foo = Bar | Baz"
        # parseDecl
        # recResToMaybe
        >>= getPursDef
        # shouldEqual (Just $ DefData (Name "Foos"))

  describe "" do
    it "" do
      [ PursModule (ModuleName "My")
          [ DefData (Name "Foo") ]
      ]
        # genInstances
        # runImportWriterM
        # fst
        # printDecls
        # Str.split (Pattern "\n")
        # shouldEqual $
        [ "instance ToTsBridge My.Foo where"
        , "  toTsBridge = tsOpaqueType \"My\" \"Foo\""
        ]

--   describe "Program Printing" do
--     let
--       files =
--         [ [ "module Foo where"
--           , "data Bar = Baz"
--           ]
--         ]

--       file1 =
--         [ "module My where"
--         , "{-GEN:instances-}"
--         , "{-GEN:END-}"
--         ]

--       file =
--         [ "module My where"
--         , "{-GEN:instances-}"
--         , "instance TsBridge Foo.Bar where"
--         , "  toTsBridge = tsOpaqueType \"Foo\" \"Bar\""
--         , "{-GEN:END-}"
--         ]

--     it "" do
--       2 `shouldEqual` 2