module TsBridgeGen.Print where

import Prelude

import Control.Monad.Writer (Writer, WriterT, runWriter, runWriterT, tell)
import Data.Array as A
import Data.Array.NonEmpty (cons', foldr1, (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), Replacement(..))
import Data.String as Str
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Undefined (undefined)
import Debug (spy)
import Dodo as Dodo
import Language.PS.CST (Declaration(..), Expr(..), Guarded(..), Ident(..), InstanceBinding(..), PSConstraint(..), PSType(..), ProperName(..), ProperNameType_TypeConstructor, QualifiedName, mkModuleName, nonQualifiedName, printDeclaration, qualifiedName)
import TsBridgeGen.Types (Import(..), ModuleName(..), Name(..), PursDef(..), PursModule(..), UnsupportedScope(..))

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
array ss = "[" <> Str.joinWith "," ss <> "]"

items = Str.joinWith " "

itemsWithParens xs | Just { head, tail: [] } <- A.uncons xs = head
itemsWithParens xs = parens $ items xs

genInstances :: forall m. Monad m => Array PursModule -> ImportWriterT m (Array PursCodeSnippet)
genInstances modules = sequence do
  (PursModule mn@(ModuleName mn') defs) <- modules
  pursDef <- defs
  case pursDef of
    DefData n@(Name n') args -> [ genInstance "defaultOpaqueType" mn n args ]

    DefNewtype n@(Name n') args -> [ genInstance "defaultNewtype" mn n args ]

    DefUnsupported (Name n) BothExportAndInstance reason ->
      [ pure $ CodeSnipComments [ "auto generated instance for `" <> n <> "` is not supported: " <> reason ] ]

    _ ->
      []

genInstance :: forall m. Monad m => String -> ModuleName -> Name -> Array Name -> ImportWriterT m PursCodeSnippet
genInstance fn mn@(ModuleName mn') (Name n') args =  do
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
        , itemsWithParens ([ "Auto." <> mn' <> "." <> n' ] <> (unwrap <$> args))
        , "where"
        , "toTsBridge"
        , "="
        , "defaultOpaqueType"
        , str mn'
        , str n'
        , array $ (Str.toUpper <<< str <<< unwrap) <$> args
        , array $
            ( ( \x -> items
                  [ "toTsBridge"
                  , parens $ items [ "Proxy", "::", "_", x ]
                  ]
              ) <<< unwrap
            ) <$> args
        ]
    ]

-- pure do
--   tell
--     { imports: Set.singleton $ ImportAuto
--         { from: mn
--         , as: Name ("Auto." <> mn')
--         }
--     }
--   pure $ CodeSnipDecls $
--     genTsBridgeInstance mn n
--       ( (ExprIdent $ nonQualifiedName (Ident "tsNewtype"))
--           `ExprApp` (ExprString mn')
--           `ExprApp` (ExprString n')
--           `ExprApp` (ExprArray [])
--       )

genTsProgram :: forall m. Monad m => Array PursModule -> ImportWriterT m String
genTsProgram = genTsProgram' >>> map printPursSnippets

genTsProgram' :: forall m. Monad m => Array PursModule -> ImportWriterT m (Array PursCodeSnippet)
genTsProgram' modules = do
  ms <- modules
    # traverse genTsModuleFile
    <#> ExprArray

  let
    type_ = TypeConstructor $ nonQualifiedName $ ProperName "TsProgram"
    valueName = Ident "generatedTsProgram"

  let
    signature = DeclSignature
      { comments: Nothing, ident: valueName, type_ }

  let
    body = ExprIdent (nonQualifiedName $ Ident "tsProgram") `ExprApp` ms

  let
    valueDef = DeclValue
      { comments: Nothing
      , valueBindingFields:
          { name: valueName
          , binders: []
          , guarded: Unconditional { expr: body, whereBindings: [] }
          }
      }

  pure [ CodeSnipDecls [ "signature", "valueDef" ] ]

genTsModuleFile :: forall m. Monad m => PursModule -> ImportWriterT m Expr
genTsModuleFile (PursModule mn defs) = do
  let xs = defs <#> genTsDef mn
  let ModuleName mn' = mn

  tell
    { imports: Set.singleton $ ImportAuto
        { from: mn
        , as: Name ("Auto." <> mn')
        }
    }

  pure $ ExprIdent (nonQualifiedName $ Ident "tsModuleFile")
    `ExprApp` ExprString (mn' <> "/index")
    `ExprApp` ExprArray xs

genTsDef :: ModuleName -> PursDef -> Expr
genTsDef (ModuleName mn) = case _ of
  DefType (Name n) ->
    ExprIdent (nonQualifiedName $ Ident "tsTypeAlias")
      `ExprApp` ExprString n
      `ExprApp`
        ( ExprIdent (nonQualifiedName $ Ident "toTsBridge")
            `ExprApp` genProxy
              ( TypeConstructor
                  (qualifiedName (mkModuleName $ pure mn) (ProperName n))
              )
        )

  DefValue (Name n) ->
    ExprIdent (nonQualifiedName $ Ident "tsValue")
      `ExprApp` ExprString n
      `ExprApp`
        ( ExprIdent (nonQualifiedName $ Ident "toTsBridge")
            `ExprApp` ExprIdent
              (qualifiedName (mkModuleName $ pure mn) $ Ident n)
        )

  DefData (Name n) _ ->
    ExprIdent (nonQualifiedName $ Ident "tsOpaqueType")
      `ExprApp` (ExprIdent $ nonQualifiedName $ Ident "Mp")
      `ExprApp` ExprString n
      `ExprApp`
        ( genProxy (TypeConstructor (qualifiedName (mkModuleName $ "Auto" : pure mn) (ProperName n)))
        )

  DefNewtype (Name n) _ ->
    ExprIdent (nonQualifiedName $ Ident "tsNewtype")
      `ExprApp` ExprString n
      `ExprApp`
        ( ExprIdent (nonQualifiedName $ Ident "toTsBridge")
            `ExprApp` genProxy
              (TypeConstructor (qualifiedName (mkModuleName $ pure mn) (ProperName n)))
        )

  DefUnsupported name JustExport reason -> unsupported name reason

  DefUnsupported name BothExportAndInstance reason -> unsupported name reason

unsupported :: Name -> String -> Expr
unsupported (Name n) reason = ExprIdent (nonQualifiedName $ Ident "tsUnsupported")
  `ExprApp` ExprString n
  `ExprApp` ExprString reason

genProxy qn = ExprTyped
  (ExprConstructor $ nonQualifiedName $ ProperName "Proxy")
  (TypeWildcard `TypeApp` qn)

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

genTsBridgeInstance :: ModuleName -> Name -> Array Name -> Expr -> Declaration
genTsBridgeInstance (ModuleName mn) (Name n) args expr = DeclInstanceChain
  { comments: Nothing
  , instances: pure { head, body }
  }
  where

  head =
    { instName: Ident instName.name
    , instConstraints: (spy "args" args) <#> \(Name n) -> PSConstraint
        { className: nonQualifiedName $ ProperName "ToTsBridge"
        , args: [ TypeVar $ Ident n ]
        }
    , instClass: nonQualifiedName
        (ProperName "ToTsBridge")
    , instTypes: pure $ foldr1 TypeApp $
        cons'
          (TypeConstructor $ qualifiedName (mkModuleName $ pure ("Auto." <> mn)) (ProperName n))
          (TypeVar <<< Ident <<< unwrap <$> args)
    }

  body = pure $ InstanceBindingName
    { binders: []
    , name: Ident "toTsBridge"
    , guarded: Unconditional
        { expr
        , whereBindings: []
        }
    }
