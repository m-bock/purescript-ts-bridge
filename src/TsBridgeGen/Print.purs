module TsBridgeGen.Print where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Writer (class MonadTell, class MonadTrans, Writer, WriterT, lift, runWriter, runWriterT, tell)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), Replacement(..))
import Data.String as Str
import Data.Traversable (sequence, traverse)
import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Undefined (undefined)
import Dodo as Dodo
import Effect.Class (class MonadEffect)
import Language.PS.CST (Comments, Declaration(..), Expr(..), Guarded(..), Ident(..), InstanceBinding(..), PSType(..), ProperName(..), ProperNameType_TypeConstructor, QualifiedName, mkModuleName, nonQualifiedName, printComments, printDeclaration, printDeclarations, qualifiedName)
import TsBridge (TsDeclaration)
import TsBridgeGen.Monad (class MonadLog, class MonadWarn, log, warn, warnCount)
import TsBridgeGen.Types (AppLog, AppWarning, Import(..), ModuleName(..), Name(..), PursDef(..), PursModule(..))

data PursCodeSnippet
  = CodeSnipDecl Declaration
  | CodeSnipComments (Array String)

newtype ImportWriterM a = ImportWriterM (Writer Accum a)
newtype ImportWriterT m a = ImportWriterT (WriterT Accum m a)

type Accum = { imports :: Set Import }

derive newtype instance Bind m => Bind (ImportWriterT m)
derive newtype instance Monad m => Monad (ImportWriterT m)
derive newtype instance Apply m => Apply (ImportWriterT m)
derive newtype instance Applicative m => Applicative (ImportWriterT m)
derive newtype instance Functor m => Functor (ImportWriterT m)
derive newtype instance MonadRec m => MonadRec (ImportWriterT m)
derive newtype instance Monad m => MonadTell Accum (ImportWriterT m)
derive newtype instance MonadTrans ImportWriterT
derive newtype instance MonadThrow e m => MonadThrow e (ImportWriterT m)
derive newtype instance MonadError e m => MonadError e (ImportWriterT m)
derive newtype instance MonadEffect m => MonadEffect (ImportWriterT m)

instance MonadWarn AppWarning m => MonadWarn AppWarning (ImportWriterT m) where
  warn = warn >>> lift
  warnCount = warnCount # lift

instance MonadLog AppLog m => MonadLog AppLog (ImportWriterT m) where
  log = log >>> lift

runImportWriterM :: forall a. ImportWriterM a -> a /\ { imports :: Set Import }
runImportWriterM (ImportWriterM ma) = runWriter ma

runImportWriterT :: forall m a. ImportWriterT m a -> m (a /\ { imports :: Set Import })
runImportWriterT (ImportWriterT ma) = runWriterT ma

genInstances :: forall m. Monad m => Array PursModule -> ImportWriterT m (Array PursCodeSnippet)
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
        pure $ CodeSnipDecl $
          genTsBridgeInstance mn n
            ( (ExprIdent $ nonQualifiedName (Ident "tsOpaqueType"))
                `ExprApp` (ExprString mn')
                `ExprApp` (ExprString n')
            )

    DefType _ -> []

    DefValue _ -> []

    DefUnsupportedInstAndExport (Name n) reason -> [ pure $ CodeSnipComments [ "`" <> n <> "` is unsupported: " <> reason ] ]

    DefUnsupportedExport _ _ -> []

    DefNewtype n@(Name n') ->
      []

-- pure do
--   tell
--     { imports: Set.singleton $ ImportAuto
--         { from: mn
--         , as: Name ("Auto." <> mn')
--         }
--     }
--   pure $ CodeSnipDecl $
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
    signature = CodeSnipDecl $ DeclSignature
      { comments: Nothing, ident: valueName, type_ }

  let
    body = ExprIdent (nonQualifiedName $ Ident "tsProgram") `ExprApp` ms

  let
    valueDef = CodeSnipDecl $ DeclValue
      { comments: Nothing
      , valueBindingFields:
          { name: valueName
          , binders: []
          , guarded: Unconditional { expr: body, whereBindings: [] }
          }
      }

  pure [ signature, valueDef ]

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

  DefUnsupportedInstAndExport name reason -> unsupported name reason
  DefUnsupportedExport name reason -> unsupported name reason

unsupported (Name n) reason = ExprIdent (nonQualifiedName $ Ident "tsUnsupported")
  `ExprApp` ExprString n
  `ExprApp` ExprString reason

genProxy :: QualifiedName (ProperName ProperNameType_TypeConstructor) -> Expr
genProxy qn = ExprTyped
  (ExprConstructor $ nonQualifiedName $ ProperName "Proxy")
  (TypeWildcard `TypeApp` TypeConstructor qn)

printPursSnippets :: Array PursCodeSnippet -> String
printPursSnippets = Str.joinWith "\n\n" <<< map case _ of
  CodeSnipDecl d -> printDeclaration d
    # Dodo.print Dodo.plainText Dodo.twoSpaces
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
