module TsBridgeGen.Print where

import Prelude

import Control.Monad.Writer (Writer, WriterT, runWriter, runWriterT, tell)
import Data.Array (catMaybes)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), Replacement(..), toLower)
import Data.String as Str
import Data.Traversable (sequence, traverse)
import Data.Tuple.Nested (type (/\))
import TsBridgeGen.Types (Import(..), ModuleName(..), Name(..), PursDef(..), PursModule(..), TypeAnn, UnsupportedScope(..))

data PursCodeSnippet
  = CodeSnipDecls (Array String)
  | CodeSnipComments (Array String)

type ImportWriterM a = Writer Accum a
type ImportWriterT m a = WriterT Accum m a

type Accum = { imports :: Set Import }

runImportWriterM :: forall a. ImportWriterM a -> a /\ { imports :: Set Import }
runImportWriterM ma = runWriter ma

runImportWriterT :: forall m a. ImportWriterT m a -> m (a /\ { imports :: Set Import })
runImportWriterT ma = runWriterT ma

str :: String -> String
str s = "\"" <> s <> "\""

parens :: String -> String
parens s = "(" <> s <> ")"

instConstraints :: Array String -> String
instConstraints [] = ""
instConstraints xs = "(" <> Str.joinWith "," xs <> (items [ ")", "=>" ])

array :: Array String -> String
array ss = "[" <> Str.joinWith ", " ss <> "]"

items :: Array String -> String
items = Str.joinWith " "

itemsWithParens :: Array String -> String
itemsWithParens xs | Just { head, tail: [] } <- A.uncons xs = head
itemsWithParens xs = parens $ items xs

genInstances :: forall m. Monad m => Array PursModule -> ImportWriterT m (Array PursCodeSnippet)
genInstances modules = sequence do
  (PursModule mn defs) <- modules
  pursDef <- defs
  case pursDef of
    DefData n args -> [ genInstance "defaultOpaqueType" mn n args ]

    DefNewtype n args -> [ genInstance "defaultNewtype" mn n args ]

    DefUnsupported (Name n) BothExportAndInstance reason ->
      [ pure $ CodeSnipComments [ "auto generated instance for `" <> n <> "` is not supported: " <> reason ] ]

    _ ->
      []

genInstance :: forall m. Monad m => String -> ModuleName -> Name -> Array Name -> ImportWriterT m PursCodeSnippet
genInstance fn mn@(ModuleName mn') n@(Name n') args = do
  tell
    { imports: Set.singleton $ ImportAuto
        { from: mn
        , as: Name ("Auto." <> mn')
        }
    }
  pure $ CodeSnipDecls
    [ items
        [ "instance"
        , instConstraints $ ((\x -> items [ "ToTsBridge", x ]) <<< unwrap <$> args)
        , "ToTsBridge"
        , itemsWithParens ([ genQualNameAuto mn n ] <> (unwrap <$> args))
        , "where"
        , "toTsBridge"
        , "="
        , fn
        , str mn'
        , str n'
        , array $ (Str.toUpper <<< str <<< unwrap) <$> args
        , array $
            ( unwrap >>> \x -> items
                [ "toTsBridge", genProxy [ x ] ]
            ) <$> args
        ]
    ]

genTypeAnn :: TypeAnn -> Array String
genTypeAnn _ = []

genQualNameAuto :: ModuleName -> Name -> String
genQualNameAuto (ModuleName mn) (Name n) = "Auto." <> mn <> "." <> n

genProxy :: Array String -> String
genProxy xs = parens $ items [ "Proxy", "::", "_", itemsWithParens xs ]

genProxy' :: PursTok -> PursTok
genProxy' t = PursTokItems
  [ PursTokName $ Name "Proxy"
  , PursTokDblColn
  , PursTokWildcard
  , t
  ]

data PursTok
  = PursTokInt Int
  | PursTokStr String
  | PursTokName Name
  | PursTokDblColon
  | PursTokWildcard
  | PursTokDblColn
  | PursTokArray (Array PursTok)
  | PursTokDecls (Array PursTok)
  | PursTokItems (Array PursTok)

genTsProgram :: forall m. Monad m => Array PursModule -> ImportWriterT m String
genTsProgram = genTsProgram' >>> map printPursSnippets

genTsProgram' :: forall m. Monad m => Array PursModule -> ImportWriterT m (Array PursCodeSnippet)
genTsProgram' modules = do
  ms <- modules
    # traverse genTsModuleFile

  pure
    [ CodeSnipDecls
        [ items [ "generatedTsProgram", "::", "TsProgram" ]
        , items [ "generatedTsProgram", "=", "tsProgram", array ms ]
        ]
    ]

genTsModuleFile :: forall m. Monad m => PursModule -> ImportWriterT m String
genTsModuleFile (PursModule mn defs) = do
  let xs = defs <#> genTsDef mn
  let ModuleName mn' = mn

  tell
    { imports: Set.singleton $ ImportAuto
        { from: mn
        , as: Name ("Auto." <> mn')
        }
    }

  pure $ items [ "tsModuleFile", str (mn' <> "/index"), array $ catMaybes xs ]

-- ExprIdent (nonQualifiedName $ Ident "tsModuleFile")
--   `ExprApp` ExprString (mn' <> "/index")
--   `ExprApp` ExprArray xs

genVar :: Name -> Array String
genVar (Name s) = [ "Var", str s ]

genTsDef :: ModuleName -> PursDef -> Maybe String
genTsDef mn@(ModuleName mn') = case _ of
  DefValue n@(Name n') _ -> Just $
    items [ "tsValue", "Mp", str n', parens $ items [ genQualNameAuto mn n ] ]

  DefData n@(Name n') args -> Just $
    items [ "tsOpaqueType", "Mp", str n', genProxy ([ genQualNameAuto mn n ] <> (itemsWithParens <<< genVar <$> args)) ]

  -- DefNewtype n@(Name n') _ -> Just $
  --   items [ "tsNewtype", "Mp", str n', genProxy [genQualNameAuto mn n]  ]

  -- DefType n@(Name n') -> Just $
  --   items [ "tsTypeAlias", "Mp", str n', genProxy [genQualNameAuto mn n]  ]

  DefUnsupported name JustExport reason -> Just $ unsupported mn name reason

  DefUnsupported name BothExportAndInstance reason -> Just $ unsupported mn name reason

  _ -> Nothing

--  case _ of
--   DefType (Name n) ->
--     ExprIdent (nonQualifiedName $ Ident "tsTypeAlias")
--       `ExprApp` ExprString n
--       `ExprApp`
--         ( ExprIdent (nonQualifiedName $ Ident "toTsBridge")
--             `ExprApp` genProxy
--               ( TypeConstructor
--                   (qualifiedName (mkModuleName $ pure mn) (ProperName n))
--               )
--         )

--   DefData (Name n) _ ->
--     ExprIdent (nonQualifiedName $ Ident "tsOpaqueType")
--       `ExprApp` (ExprIdent $ nonQualifiedName $ Ident "Mp")
--       `ExprApp` ExprString n
--       `ExprApp`
--         ( genProxy (TypeConstructor (qualifiedName (mkModuleName $ "Auto" : pure mn) (ProperName n)))
--         )

--   DefNewtype (Name n) _ ->
--     ExprIdent (nonQualifiedName $ Ident "tsNewtype")
--       `ExprApp` ExprString n
--       `ExprApp`
--         ( ExprIdent (nonQualifiedName $ Ident "toTsBridge")
--             `ExprApp` genProxy
--               (TypeConstructor (qualifiedName (mkModuleName $ pure mn) (ProperName n)))
--         )

unsupported :: ModuleName -> Name -> String -> String
unsupported (ModuleName mn) (Name n) reason = items
  [ "tsUnsupported"
  , str n
  , str reason
  , if startsLower n then "unit" else "unit"
  ]
  where
  ref = "Auto" <> "." <> mn <> "." <> n
  isLower s = toLower s == s
  startsLower s = isLower $ Str.take 1 s

printPursSnippets :: Array PursCodeSnippet -> String
printPursSnippets =
  Str.joinWith "\n\n" <<< map case _ of
    CodeSnipDecls ds -> ds
      -- <#> printDeclaration
      --   >>> Dodo.print Dodo.plainText
      --     { pageWidth: 300, ribbonRatio: 1.0, indentUnit: "  ", indentWidth: 2 }
      # Str.joinWith "\n"
      # instName.replace
    CodeSnipComments cs -> cs <#> ("-- " <> _) # Str.joinWith "\n"

printImports :: Set Import -> String
printImports x = x
  # (Set.toUnfoldable :: _ -> Array _)
  <#> printImport
  # Str.joinWith "\n"

printImport :: Import -> String
printImport = case _ of
  ImportAuto { from: ModuleName mn, as: Name n } ->
    "import " <> mn <> " as " <> n
  ImportUser s -> s

instName :: { name :: String, replace :: String -> String }
instName = { name, replace }
  where
  name = "instName"
  replace =
    Str.replaceAll (Pattern (" " <> name <> " ::")) (Replacement "")
      >>> Str.replaceAll (Pattern (" " <> name <> "\n  ::")) (Replacement "")

