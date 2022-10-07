module TsBridgeGenSpec where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Writer (WriterT, tell)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (Pattern(..))
import Data.String as Str
import Data.Tuple (Tuple, fst)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Undefined (undefined)
import Debug (spy)
import PureScript.CST (RecoveredParserResult(..), parseDecl)
import Test.Spec (Spec, describe, it)
import Test.TsBridgeGen.Monad (TestMResult(..), defaultTestCapabilities, defaultTestConfig, runTestM, runTestM_)
import Test.Util (shouldEqual)
import TsBridgeGen (class MonadApp, class MonadLog, AppEffects(..), AppEnv(..), AppError, AppLog, AppWarning, ModuleName(..), Name(..), PursDef(..), PursModule(..), genInstances, getPursDef, printPursSnippets, runImportWriterM, runImportWriterT)
import TsBridgeGen.Cli (patchClassFile, patchModulesFile)
import TsBridgeGen.Config (AppConfig(..))

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
      let
        testEnv = AppEnv
          { config: defaultTestConfig
          , capabilities: defaultTestCapabilities
          }

      patchClassFile
        "Module.purs"
        [ PursModule (ModuleName "Module1") [ DefData (Name "Foo1") ]
        , PursModule (ModuleName "Module2") [ DefData (Name "Foo2") ]
        ]
        ( Str.joinWith "\n"
            [ "module MyApp.TsBridgeClass where"
            , ""
            , "{-GEN:imports"
            , "{}"
            , "-}"
            , ""
            , "import Data.Either (Either)"
            , ""
            , "{-GEN:END-}"
            , ""
            , "{-GEN:instances"
            , "{ \"include\": [ \"**\" ]"
            , ", \"exclude\": []"
            , "}"
            , "-}"
            , ""
            , "{-GEN:END-}"
            ]
        )
        # runTestM_ testEnv
        <#> (Str.split $ Pattern "\n")
        # shouldEqual
        $
          ( TestMResult Map.empty {logs: [], errors: []}
              ( Right
                  [ "module MyApp.TsBridgeClass where"
                  , ""
                  , "{-GEN:imports"
                  , "{}"
                  , "-}"
                  , ""
                  , "import Module1 as Auto.Module1"
                  , "import Module2 as Auto.Module2"
                  , "import Data.Either (Either)"
                  , ""
                  , "{-GEN:END-}"
                  , ""
                  , "{-GEN:instances"
                  , "{ \"include\": [\"**\"], \"exclude\": [] }"
                  , "-}"
                  , ""
                  , "instance ToTsBridge Auto.Module1.Foo1 where"
                  , "  toTsBridge = defaultOpaqueType \"Module1\" \"Foo1\" [] []"
                  , ""
                  , "instance ToTsBridge Auto.Module2.Foo2 where"
                  , "  toTsBridge = defaultOpaqueType \"Module2\" \"Foo2\" [] []"
                  , ""
                  , "{-GEN:END-}"
                  ]
              )
          )

  describe "patchModulesFile" do
    it "patches a modules file correctly" do
      let
        testEnv = AppEnv
          { config: defaultTestConfig
          , capabilities: defaultTestCapabilities
          }

      patchModulesFile
        "Module.purs"
        [ PursModule (ModuleName "Module1") [ DefData (Name "Foo1") ]
        , PursModule (ModuleName "Module2") [ DefData (Name "Foo2") ]
        ]
        ( Str.joinWith "\n"
            [ "module MyApp.TsModules where"
            , ""
            , "{-GEN:imports"
            , "{ \"autoPrefix\": \"Auto\" }"
            , "-}"
            , ""
            , "import Data.Either (Either)"
            , ""
            , "{-GEN:END-}"
            , ""
            , "{-GEN:ts-program"
            , "{ \"include\": [\"**\"], \"exclude\": [] }"
            , "-}"
            , ""
            , "{-GEN:END-}"
            ]
        )
        # runTestM_ testEnv
        <#> (Str.split $ Pattern "\n")
        # shouldEqual
        $
          ( TestMResult Map.empty  {logs: [], errors: []}
              ( Right
                  [ "module MyApp.TsModules where"
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
                  , "{-GEN:ts-program"
                  , "{ \"include\": [\"**\"], \"exclude\": [] }"
                  , "-}"
                  , ""
                  , "generatedTsProgram :: TsProgram"
                  , "generatedTsProgram = tsProgram"
                  , "  [ tsModuleFile \"Module1/index\""
                  , "      [ tsOpaqueType Mp \"Foo1\" (Proxy :: _ Auto.Module1.Foo1) ]"
                  , "  , tsModuleFile \"Module2/index\""
                  , "      [ tsOpaqueType Mp \"Foo2\" (Proxy :: _ Auto.Module2.Foo2) ]"
                  , "  ]"
                  , ""
                  , "{-GEN:END-}"
                  ]
              )
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
        # shouldEqual (Just (DefUnsupportedExport (Name "x") "value")) -- (Just $ DefValue (Name "x"))

  describe "Type Alias" do
    it "parses correctly" do
      "type Foo = Int"
        # parseDecl
        # recResToMaybe
        >>= getPursDef
        # shouldEqual (Just (DefUnsupportedExport (Name "Foo") "type alias")) -- (Just $ DefType (Name "Foo"))

  describe "Newtype" do
    it "parses correctly" do
      "newtype Foo = Foo Int"
        # parseDecl
        # recResToMaybe
        >>= getPursDef
        # shouldEqual (Just (DefUnsupportedInstAndExport (Name "Foo") "newtype")) --  (Just $ DefNewtype (Name "Foo"))

  describe "Data type" do
    it "prints correctly" do
      [ PursModule (ModuleName "My")
          [ DefData (Name "Foo") ]
      ]
        # genInstances
        # runImportWriterM
        # fst
        # printPursSnippets
        # Str.split (Pattern "\n")
        # shouldEqual $
        [ "instance ToTsBridge Auto.My.Foo where"
        , "  toTsBridge = defaultOpaqueType \"My\" \"Foo\" [] []"
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
