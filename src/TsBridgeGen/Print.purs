module TsBridgeGen.Print where

import Prelude

import Control.Monad.Writer (Writer, WriterT, runWriter, runWriterT, tell)
import Data.Array (catMaybes, (:))
import Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), Replacement(..), replaceAll, toLower)
import Data.String as Str
import Data.Traversable (fold, sequence, traverse)
import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Undefined (undefined)
import TsBridgeGen.Types (Import(..), ModuleName(..), Name(..), PursDef(..), PursModule(..), TypeAnn(..), UnsupportedScope(..))

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

instConstraints :: Array PursTokTree -> Maybe PursTokTree
instConstraints [] = Nothing
instConstraints xs = Just $ PursTokItems [ PursTokList xs, PursTokFatArrow ]

array :: Array String -> String
array ss = "[" <> Str.joinWith ", " ss <> "]"

items :: Array String -> String
items = Str.joinWith " "

itemsWithParens :: Array String -> String
itemsWithParens xs | Just { head, tail: [] } <- A.uncons xs = head
itemsWithParens xs = parens $ items xs

genInstances :: forall m. Monad m => Array PursModule -> ImportWriterT m PursTokTree
genInstances modules = PursTokSections <$> sequence do
  (PursModule mn defs) <- modules
  pursDef <- defs

  case pursDef of
    DefData n args ->
      [ genInstance [ PursTokQualName nsTsb $ Name "defaultOpaqueType" ]
          mn
          n
          args
      ]

    DefNewtype n args ->
      [ genInstance
          [ PursTokQualName nsTsb $ Name "defaultBrandedType"
          , PursTokName $ Name "Mp"
          ]
          mn
          n
          args
      ]

    DefUnsupported (Name n) BothExportAndInstance reason ->

      [ pure $ PursTokLineComment ("auto generated instance for `" <> n <> "` is not supported: " <> reason) ]

    _ ->
      []

genInstance :: forall m. Monad m => Array PursTokTree -> ModuleName -> Name -> Array Name -> ImportWriterT m PursTokTree
genInstance initArgs mn@(ModuleName mn') n@(Name n') args = do
  tell
    { imports: Set.singleton $ ImportAuto
        { from: mn
        , as: Name ("Auto." <> mn')
        }
    }
  pure $
    PursTokItems
      ( [ PursTokInstance ]
          <>
            ( maybe [] pure $
                instConstraints ((\x -> PursTokItems [ PursTokName $ Name "ToTsBridge", PursTokName x ]) <$> args)
            )
          <>
            [ PursTokName $ Name "ToTsBridge"
            , PursTokGroup $ PursTokQualName (mkAuto mn) n : (PursTokName <$> args)
            , PursTokWhere
            , PursTokName $ Name "toTsBridge"
            , PursTokEqual
            ]
          <> initArgs
          <>
            [ PursTokStr mn'
            , PursTokStr n'
            , PursTokArray (PursTokStr <<< Str.toUpper <<< unwrap <$> args)
            , PursTokArray $
                ( \x -> PursTokItems
                    [ PursTokName $ Name "toTsBridge", genProxy (PursTokName x) ]
                ) <$> args
            ]
      )

genQualNameAuto :: ModuleName -> Name -> String
genQualNameAuto (ModuleName mn) (Name n) = "Auto." <> mn <> "." <> n

genProxy :: PursTokTree -> PursTokTree
genProxy t = PursTokGroup
  [ PursTokName $ Name "Proxy"
  , PursTokDblColon
  , PursTokWildcard
  , t
  ]

nsTsb :: ModuleName
nsTsb = ModuleName ("TSB")

data PursTokTree
  = PursTokInt Int
  | PursTokStr String
  | PursTokName Name
  | PursTokQualName ModuleName Name
  | PursTokDblColon
  | PursTokWildcard
  | PursTokLineComment String
  | PursTokFatArrow
  | PursTokArrow
  | PursTokInstance
  | PursTokWhere
  | PursTokEqual
  | PursTokComma
  | PursTokList (Array PursTokTree)
  | PursTokArray (Array PursTokTree)
  | PursTokDecls (Array PursTokTree)
  | PursTokSections (Array PursTokTree)
  | PursTokItems (Array PursTokTree)
  | PursTokGroup (Array PursTokTree)

printPursTokTree :: PursTokTree -> String
printPursTokTree = case _ of
  PursTokInt x -> show x
  PursTokStr x -> "\"" <> replaceAll (Pattern "\"") (Replacement "\\\"") x <> "\""
  PursTokName (Name n) -> n
  PursTokQualName (ModuleName mn) (Name n) -> mn <> "." <> n
  PursTokDblColon -> "::"
  PursTokWildcard -> "_"
  PursTokFatArrow -> "=>"
  PursTokArrow -> "->"
  PursTokLineComment x -> "-- " <> x
  PursTokInstance -> "instance"
  PursTokWhere -> "where"
  PursTokEqual -> "="
  PursTokComma -> ","
  PursTokArray xs -> fold
    [ "["
    , xs <#> printPursTokTree # Str.joinWith ","
    , "]"
    ]
  PursTokDecls xs -> xs <#> printPursTokTree # Str.joinWith "\n"
  PursTokSections xs -> xs <#> printPursTokTree # Str.joinWith "\n\n"
  PursTokGroup [ x ] -> printItems [ x ]
  PursTokGroup xs -> fold [ "(", printItems xs, ")" ]
  PursTokItems xs -> printItems xs
  PursTokList [ x ] -> printItems' [ x ]
  PursTokList xs -> fold [ "(", printItems' xs, ")" ]

  where
  printItems xs = xs <#> printPursTokTree # Str.joinWith " "
  printItems' xs = xs <#> printPursTokTree # Str.joinWith ","

genTsProgram :: forall m. Monad m => Array PursModule -> ImportWriterT m PursTokTree
genTsProgram modules = do
  ms <- modules
    # traverse genTsModuleFile

  pure
    $ PursTokSections
        [ PursTokDecls
            [ PursTokItems
                [ PursTokName $ Name "generatedTsProgram"
                , PursTokDblColon
                , PursTokQualName nsTsb $ Name "TsProgram"
                ]
            , PursTokItems
                [ PursTokName $ Name "generatedTsProgram"
                , PursTokEqual
                , PursTokItems [ PursTokQualName nsTsb $ Name "tsProgram", PursTokArray ms ]
                ]
            ]
        ]

genTsModuleFile :: forall m. Monad m => PursModule -> ImportWriterT m PursTokTree
genTsModuleFile (PursModule mn defs) = do
  let xs = defs <#> genTsDef mn
  let ModuleName mn' = mn

  tell
    { imports: Set.singleton $ ImportAuto
        { from: mn
        , as: Name ("Auto." <> mn')
        }
    }

  pure $ PursTokItems
    [ PursTokQualName nsTsb $ Name "tsModuleFile"
    , PursTokStr (mn' <> "/index")
    , PursTokArray $ catMaybes xs
    ]

-- ExprIdent (nonQualifiedName $ Ident "tsModuleFile")
--   `ExprApp` ExprString (mn' <> "/index")
--   `ExprApp` ExprArray xs

genVar :: Name -> PursTokTree
genVar (Name s) = PursTokGroup [ PursTokName (Name "Var"), PursTokStr s ]

mkAuto :: ModuleName -> ModuleName
mkAuto (ModuleName mn) = ModuleName ("Auto." <> mn)

genTypeAnn :: TypeAnn -> PursTokTree
genTypeAnn = go false
  where
  go withParens = case _ of
    TypeAnnId Nothing -> PursTokWildcard
    TypeAnnId (Just n) -> genVar n
    TypeAnnApp t1 t2 ->
      (if withParens then PursTokGroup else PursTokItems)
        [ go true t1, go true t2 ]
    TypeAnnFn t1 t2 -> PursTokItems [ go false t1, PursTokArrow, go false t2 ]
    TypeAnnRecord _ -> undefined

genTsDef :: ModuleName -> PursDef -> Maybe PursTokTree
genTsDef mn@(ModuleName mn') = case _ of
  DefValue n@(Name n') ann -> Just $
    PursTokItems
      [ PursTokName $ Name "tsValue"
      , PursTokName $ Name "Mp"
      , PursTokStr n'
      , PursTokGroup
          [ PursTokQualName (mkAuto mn) n
          , PursTokDblColon
          , fromMaybe PursTokWildcard $ genTypeAnn <$> ann
          ]
      ]

  DefData n@(Name n') args -> Just $
    PursTokItems
      [ PursTokQualName nsTsb $ Name "tsOpaqueType"
      , PursTokName $ Name "Mp"
      , PursTokStr n'
      , genProxy (PursTokGroup $ PursTokQualName (mkAuto mn) n : (genVar <$> args))
      ]

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

unsupported :: ModuleName -> Name -> String -> PursTokTree
unsupported (ModuleName mn) (Name n) reason = PursTokItems
  [ PursTokQualName nsTsb $ Name "tsUnsupported"
  , PursTokStr n
  , PursTokStr reason
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

