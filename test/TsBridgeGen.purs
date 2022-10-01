module Test.TsBridgeGen where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Writer (WriterT, tell)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (Pattern(..))
import Data.String as Str
import Data.Tuple (Tuple, fst)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Undefined (undefined)
import PureScript.CST (RecoveredParserResult(..), parseDecl)
import Test.Spec (Spec, describe, it)
import Test.Util (shouldEqual)
import TsBridgeGen (class MonadLog, class MonadWarn, AppError, AppLog, AppWarning, ModuleName(..), Name(..), PursDef(..), PursModule(..), genInstances, getPursDef, printPursSnippets, runImportWriterT)
import TsBridgeGen.Core (patchClassFile)

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

  describe "patchClassFile" do
    it "patches a class file correctly" do
      patchClassFile
        "Module.purs"
        [ PursModule (ModuleName "Module1") [ DefData (Name "Foo1") ]
        , PursModule (ModuleName "Module2") [ DefData (Name "Foo2") ]
        ]
        ( Str.joinWith "\n"
            [ "module MyApp.TsBridgeClass where"
            , ""
            , "{-GEN:imports"
            , "{ \"autoPrefix\": \"Auto\" }"
            , "-}"
            , ""
            , "import Data.Either (Either)"
            , ""
            , "{-GEN:END-}"
            , ""
            , "{-GEN:instances"
            , "{ \"include\": \"\""
            , ", \"exclude\": \"\""
            , "}"
            , "-}"
            , ""
            , "{-GEN:END-}"
            ]
        )
        # runTestM
        # lmap (map $ Str.split $ Pattern "\n")
        # shouldEqual
        $
          ( (Right
              [ "module MyApp.TsBridgeClass where"
              , ""
              , "{-GEN:imports"
              , "{ \"autoPrefix\": \"Auto\" }"
              , "-}"
              , ""
              , "import Module1 as Auto.Module1"
              , "import Module2 as Auto.Module2"
              , "import Data.Either (Either)"
              , ""
              , "{-GEN:END-}"
              , ""
              , "{-GEN:instances"
              , "{ \"include\": \"\""
              , ", \"exclude\": \"\""
              , "}"
              , "-}"
              , ""
              , "instance ToTsBridge Auto.Module1.Foo1 where"
              , "  toTsBridge = tsOpaqueType \"Module1\" \"Foo1\""
              , ""
              , "instance ToTsBridge Auto.Module2.Foo2 where"
              , "  toTsBridge = tsOpaqueType \"Module2\" \"Foo2\""
              , ""
              , "{-GEN:END-}"
              ]) /\ []
          )

  describe "Data Type" do
    it "parses correctly" do
      "data Foo = Bar | Baz"
        # parseDecl
        # recResToMaybe
        >>= getPursDef
        # shouldEqual (Just $ DefData (Name "Foo"))

  describe "Value" do
    it "parses correctly" do
      "x :: Int"
        # parseDecl
        # recResToMaybe
        >>= getPursDef
        # shouldEqual (Just $ DefValue (Name "x"))

  describe "Type Alias" do
    it "parses correctly" do
      "type Foo = Int"
        # parseDecl
        # recResToMaybe
        >>= getPursDef
        # shouldEqual (Just $ DefType (Name "Foo"))

  describe "Newtype" do
    it "parses correctly" do
      "newtype Foo = Foo Int"
        # parseDecl
        # recResToMaybe
        >>= getPursDef
        # shouldEqual (Just $ DefNewtype (Name "Foo"))

  describe "" do
    it "" do
      [ PursModule (ModuleName "My")
          [ DefData (Name "Foo") ]
      ]
        # genInstances
        # runImportWriterT
        # fst
        # printPursSnippets
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

newtype TestM a = TestM (WriterT (Array AppLog) (Either AppError) a)

instance MonadWarn AppWarning TestM where
  warn _ = pure unit
  warnCount = pure 0

derive newtype instance Bind TestM
derive newtype instance Monad TestM
derive newtype instance Apply TestM
derive newtype instance Applicative TestM
derive newtype instance Functor TestM
derive newtype instance MonadRec TestM
derive newtype instance MonadError AppError TestM
derive newtype instance MonadThrow AppError TestM


instance MonadLog AppLog TestM where
  log = TestM <<< tell <<< pure


derive instance Newtype (TestM a) _

runTestM :: forall a. TestM a -> (Tuple (Either AppError a) (Array AppLog))
runTestM = undefined -- un TestM >>> runWriterT