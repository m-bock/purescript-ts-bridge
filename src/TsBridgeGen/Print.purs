module TsBridgeGen.Print where

import Prelude

import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), Replacement(..))
import Data.String as Str
import Data.Traversable (sequence)
import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Undefined (undefined)
import Dodo as Dodo
import Language.PS.CST as CST
import TsBridgeGen.Types (ModuleName(..), Name(..), PursDef(..), PursModule(..))

type ImportWriterM a = Writer { imports :: Set String } a

runImportWriterM :: forall a. ImportWriterM a -> a /\ String
runImportWriterM = runWriter >>> map (_.imports >>> Set.toUnfoldable >>> Str.joinWith "\n")

genInstances :: Array PursModule -> ImportWriterM String
genInstances modules = printDecls <$> sequence do
  (PursModule mn@(ModuleName mn') defs) <- modules
  pursDef <- defs
  case pursDef of
    DefData n ->
      [ do
          tell { imports: Set.singleton ("import " <> mn' <> " as " <> mn') }
          pure $ genOpaqueInstance mn n
      ]
    _ -> []

genTsProgram :: Array PursModule -> ImportWriterM String
genTsProgram modules = printDecls <<< wrapProgram <$> sequence do
  (PursModule mn@(ModuleName mn') defs) <- modules
  pursDef <- defs
  case pursDef of
    DefValue n ->
      [ do
          tell { imports: Set.singleton ("import " <> mn' <> " as " <> mn') }
          pure $ gen mn n
      ]
    _ -> []
  where
    wrapProgram :: Array CST.Expr -> Array CST.Declaration
    wrapProgram = undefined

gen :: ModuleName -> Name -> CST.Expr
gen = undefined

printDecls :: Array CST.Declaration -> String
printDecls = CST.printDeclarations
  >>> Dodo.print Dodo.plainText Dodo.twoSpaces
  >>> instName.replace

instName :: { name :: String, replace :: String -> String }
instName = { name, replace }
  where
  name = "instName"
  replace = 
     Str.replaceAll (Pattern (" " <> name <> " ::")) (Replacement "")
     >>> Str.replaceAll (Pattern (" " <> name <> "\n  ::")) (Replacement "")

genOpaqueInstance :: ModuleName -> Name -> CST.Declaration
genOpaqueInstance (ModuleName mn) (Name n) = CST.DeclInstanceChain
  { comments: Nothing
  , instances: pure { head, body: [ fn ] }
  }
  where

  head =
    { instName: CST.Ident instName.name
    , instConstraints: []
    , instClass: CST.nonQualifiedName
        (CST.ProperName "ToTsBridge")
    , instTypes: pure $ CST.TypeConstructor $ CST.qualifiedName
        (CST.mkModuleName $ NonEmptyArray [ mn ])
        (CST.ProperName n)
    }

  fn = CST.InstanceBindingName
    { binders: []
    , name: CST.Ident "toTsBridge"
    , guarded: CST.Unconditional
        { expr:
            (CST.ExprIdent $ CST.nonQualifiedName (CST.Ident "tsOpaqueType"))
              `CST.ExprApp` (CST.ExprString mn)
              `CST.ExprApp` (CST.ExprString n)
              `CST.ExprApp` (CST.ExprArray [])

        , whereBindings: []
        }
    }
