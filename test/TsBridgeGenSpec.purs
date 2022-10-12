module TsBridgeGenSpec where

import Prelude

import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as Str
import Data.Tuple (fst)
import Dodo (twoSpaces)
import Dodo as Dodo
import PureScript.CST (RecoveredParserResult(..), parseDecl, parseModule)
import Test.Spec (Spec, describe, it)
import Test.TsBridgeGen.Monad (TestMResult(..), defaultTestCapabilities, defaultTestConfig, runTestM_)
import Test.Util (shouldEqual)
import Tidy (defaultFormatOptions, formatDecl, formatModule)
import Tidy.Doc (FormatDoc(..))
import TsBridgeGen (AppEnv(..), ModuleName(..), Name(..), PursDef(..), PursModule(..), TypeAnn(..), UnsupportedScope(..), genInstances, getPursDef, printPursTokTree, runImportWriterM)
import TsBridgeGen.Cli (patchClassFile, patchModulesFile)

recResToMaybe :: forall f. RecoveredParserResult f -> Maybe (f Void)
recResToMaybe = case _ of
  ParseSucceeded x -> Just x
  _ -> Nothing

spec :: Spec Unit
spec = 
  describe "TsBridgeGenSpec" do
    describe "patchClassFile" do
      it "patches a class file correctly" do
        let
          testEnv = AppEnv
            { config: defaultTestConfig
            , capabilities: defaultTestCapabilities
            }

        patchClassFile
          "Module.purs"
          [ PursModule (ModuleName "Module1") [ DefData (Name "Foo1") [] ]
          , PursModule (ModuleName "Module2") [ DefData (Name "Foo2") [] ]
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
            ( TestMResult Map.empty { logs: [], errors: [] }
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
                    , "  toTsBridge = TSB.defaultOpaqueType \"Module1\" \"Foo1\" [] []"
                    , ""
                    , "instance ToTsBridge Auto.Module2.Foo2 where"
                    , "  toTsBridge = TSB.defaultOpaqueType \"Module2\" \"Foo2\" [] []"
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
          [ PursModule (ModuleName "Module1") [ DefData (Name "Foo1") [] ]
          , PursModule (ModuleName "Module2") [ DefData (Name "Foo2") [] ]
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
            ( TestMResult Map.empty { logs: [], errors: [] }
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
                    , "generatedTsProgram :: TSB.TsProgram"
                    , "generatedTsProgram = TSB.tsProgram"
                    , "  [ TSB.tsModuleFile \"Module1/index\""
                    , "      [ TSB.tsOpaqueType Mp \"Foo1\" (Proxy :: _ Auto.Module1.Foo1) ]"
                    , "  , TSB.tsModuleFile \"Module2/index\""
                    , "      [ TSB.tsOpaqueType Mp \"Foo2\" (Proxy :: _ Auto.Module2.Foo2) ]"
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
          # shouldEqual (Just $ DefData (Name "Foo") [])

    describe "Value" do
      it "parses correctly" do
        "x :: Int"
          # parseDecl
          # recResToMaybe
          >>= getPursDef
          # shouldEqual (Just $ DefValue (Name "x") (Just (TypeAnnId Nothing)))

    describe "Type Alias" do
      it "parses correctly" do
        "type Foo = Int"
          # parseDecl
          # recResToMaybe
          >>= getPursDef
          # shouldEqual (Just (DefUnsupported (Name "Foo") JustExport "type alias")) -- (Just $ DefType (Name "Foo"))

    describe "Newtype" do
      it "parses correctly" do
        "newtype Foo = Foo Int"
          # parseDecl
          # recResToMaybe
          >>= getPursDef
          # shouldEqual (Just $ DefNewtype (Name "Foo") [])

    describe "Data type" do
      it "prints correctly" do
        [ PursModule (ModuleName "My")
            [ DefData (Name "Foo") [] ]
        ]
          # genInstances
          # runImportWriterM
          # fst
          # printPursTokTree
          # tidyPursDecl
          <#> Str.split (Pattern "\n")
          # shouldEqual $ Just
          [ "instance ToTsBridge Auto.My.Foo where"
          , "  toTsBridge = TSB.defaultOpaqueType \"My\" \"Foo\" [] []"
          ]

tidyPursDecl :: String -> Maybe String
tidyPursDecl str = str # parseDecl >>> case _ of
  ParseSucceeded d ->
    d
      # formatDecl defaultFormatOptions
          >>> (\(FormatDoc { doc }) -> doc)
          >>> Dodo.print Dodo.plainText Dodo.twoSpaces
      # Just
  _ -> Nothing

tidyPurs :: String -> Maybe String
tidyPurs str = str # parseModule >>> case _ of
  ParseSucceeded m ->
    formatModule defaultFormatOptions m
      # (\(FormatDoc { doc }) -> doc)
      # Dodo.print Dodo.plainText
          twoSpaces
      # Just
  _ -> Nothing
