module TsBridgeGen.Print where

import Prelude

import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), Replacement(..))
import Data.String as Str
import Data.Traversable (sequence, traverse)
import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Undefined (undefined)
import Dodo as Dodo
import Language.PS.CST (Declaration(..), Expr(..), Guarded(..), Ident(..), InstanceBinding(..), PSType(..), ProperName(..), ProperNameType_TypeConstructor, QualifiedName, mkModuleName, nonQualifiedName, printDeclaration, printDeclarations, qualifiedName)
import TsBridgeGen.Types (Import(..), ModuleName(..), Name(..), PursDef(..), PursModule(..))

type ImportWriterM a = Writer { imports :: Set Import } a

runImportWriterM :: forall a. ImportWriterM a -> a /\ { imports :: Set Import }
runImportWriterM = runWriter

genInstances :: Array PursModule -> ImportWriterM (Array Declaration)
genInstances modules = sequence do
  (PursModule mn@(ModuleName mn') defs) <- modules
  pursDef <- defs
  case pursDef of
    DefData n@(Name n') ->
      pure do
        tell
          { imports: Set.singleton $ ImportAuto
              { from: mn
              , as: Name ("Auto." <> mn')
              }
          }
        pure $
          genTsBridgeInstance mn n
            ( (ExprIdent $ nonQualifiedName (Ident "tsOpaqueType"))
                `ExprApp` (ExprString mn')
                `ExprApp` (ExprString n')
            )

    DefNewtype n@(Name n') ->
      pure do
        tell
          { imports: Set.singleton $ ImportAuto
              { from: mn
              , as: Name ("Auto." <> mn')
              }
          }
        pure $
          genTsBridgeInstance mn n
            ( (ExprIdent $ nonQualifiedName (Ident "tsNewtype"))
                `ExprApp` (ExprString mn')
                `ExprApp` (ExprString n')
                `ExprApp` (ExprArray [])
            )

    _ -> mempty

genTsProgram :: Array PursModule -> ImportWriterM String
genTsProgram = genTsProgram' >>> map printDecls

genTsProgram' :: Array PursModule -> ImportWriterM (Array Declaration)
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

  pure [ signature, valueDef ]

genTsModuleFile :: PursModule -> ImportWriterM Expr
genTsModuleFile (PursModule mn defs) = do
  let xs = defs <#> genTsDef mn
  let ModuleName mn' = mn

  tell
    { imports: Set.singleton $ ImportAuto
        { from: mn
        , as: Name ("Auto." <> mn')
        }
    }

  pure $ ExprIdent (nonQualifiedName $ Ident "toTsModuleFile")
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
              (qualifiedName (mkModuleName $ pure mn) (ProperName n))
        )

  DefValue (Name n) ->
    ExprIdent (nonQualifiedName $ Ident "tsValue")
      `ExprApp` ExprString n
      `ExprApp`
        ( ExprIdent (nonQualifiedName $ Ident "toTsBridge")
            `ExprApp` ExprIdent
              (qualifiedName (mkModuleName $ pure mn) $ Ident n)
        )

  DefData (Name n) ->
    ExprIdent (nonQualifiedName $ Ident "tsType")
      `ExprApp` ExprString n
      `ExprApp`
        ( ExprIdent (nonQualifiedName $ Ident "toTsBridge")
            `ExprApp` genProxy
              (qualifiedName (mkModuleName $ pure mn) (ProperName n))
        )

  DefNewtype (Name n) ->
    ExprIdent (nonQualifiedName $ Ident "tsType")
      `ExprApp` ExprString n
      `ExprApp`
        ( ExprIdent (nonQualifiedName $ Ident "toTsBridge")
            `ExprApp` genProxy
              (qualifiedName (mkModuleName $ pure mn) (ProperName n))
        )

genProxy :: QualifiedName (ProperName ProperNameType_TypeConstructor) -> Expr
genProxy qn = ExprTyped
  (ExprConstructor $ nonQualifiedName $ ProperName "Proxy")
  (TypeWildcard `TypeApp` TypeConstructor qn)

printDecls :: Array Declaration -> String
printDecls = printDeclarations
  >>> Dodo.print Dodo.plainText Dodo.twoSpaces
  >>> instName.replace

printDecl :: Declaration -> String
printDecl = printDeclaration
  >>> Dodo.print Dodo.plainText Dodo.twoSpaces
  >>> instName.replace

printImports :: Set Import -> String
printImports x = x
  # (Set.toUnfoldable :: _ -> Array _)
  <#> printImport
  # Str.joinWith "\n"

printImport :: Import -> String
printImport = case _ of
  ImportAuto { from: ModuleName mn, as: Name n } ->
    "import " <> mn <> " as " <> "Auto." <> n
  ImportUser s -> s

instName :: { name :: String, replace :: String -> String }
instName = { name, replace }
  where
  name = "instName"
  replace =
    Str.replaceAll (Pattern (" " <> name <> " ::")) (Replacement "")
      >>> Str.replaceAll (Pattern (" " <> name <> "\n  ::")) (Replacement "")

genTsBridgeInstance :: ModuleName -> Name -> Expr -> Declaration
genTsBridgeInstance (ModuleName mn) (Name n) expr = DeclInstanceChain
  { comments: Nothing
  , instances: pure { head, body }
  }
  where

  head =
    { instName: Ident instName.name
    , instConstraints: []
    , instClass: nonQualifiedName
        (ProperName "ToTsBridge")
    , instTypes: pure $ TypeConstructor $ qualifiedName
        (mkModuleName $ pure ("Auto." <> mn))
        (ProperName n)
    }

  body = pure $ InstanceBindingName
    { binders: []
    , name: Ident "toTsBridge"
    , guarded: Unconditional
        { expr
        , whereBindings: []
        }
    }
