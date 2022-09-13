module TsBridge.Monad
  ( TsBridgeAccum
  , TsBridgeM(..)
  , opaqueType
  , runTsBridgeM
  ) where

import Prelude

import Control.Monad.Writer (class MonadTell, Writer, runWriter, tell)
import Data.Array (mapWithIndex, (:))
import Data.Maybe (Maybe(..), optional)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (sequence)
import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Undefined (undefined)
import TsBridge.DTS (TsDeclaration(..), TsFilePath(..), TsImport(..), TsModule(..), TsModuleAlias(..), TsModuleFile(..), TsModulePath(..), TsName(..), TsQualName(..), TsRecordField(..), TsType(..), TsTypeArgs(..))
import TsBridge.Print (printTsName)

-------------------------------------------------------------------------------
-- Types / TsBridge
-------------------------------------------------------------------------------

newtype TsBridgeM a = TsBridgeM (Writer TsBridgeAccum a)

type TsBridgeAccum =
  { typeDefs :: Array TsModuleFile
  , imports :: Set TsImport
  , scope :: Set TsName
  }

derive newtype instance MonadTell TsBridgeAccum TsBridgeM
derive newtype instance Monad TsBridgeM
derive newtype instance Bind TsBridgeM
derive newtype instance Functor TsBridgeM
derive newtype instance Apply TsBridgeM
derive newtype instance Applicative TsBridgeM

runTsBridgeM :: forall a. TsBridgeM a -> a /\ TsBridgeAccum
runTsBridgeM (TsBridgeM ma) = runWriter ma

-------------------------------------------------------------------------------
-- Util
-------------------------------------------------------------------------------

opaqueType :: TsFilePath -> TsModuleAlias -> TsName -> Array TsName -> Array (TsBridgeM TsType) -> TsBridgeM TsType
opaqueType filePath moduleAlias name targs args' = do
  args <- sequence args'

  let
    imports = Set.singleton $
      TsImport
        moduleAlias
        (filePathToModulePath filePath)

    typeDefs =
      [ TsModuleFile
          filePath
          ( TsModule Set.empty
              [ mkOpaqueTypeDecl name targs
              ]
          )
      ]

  tell
    { typeDefs, imports, scope: Set.empty }
  pure $ TsTypeConstructor (TsQualName (Just moduleAlias) name) (TsTypeArgs args)

mkOpaqueTypeDecl :: TsName -> Array TsName -> TsDeclaration
mkOpaqueTypeDecl name args = TsDeclTypeDef name args $
  TsTypeRecord
    (opaqueField : mapWithIndex mkArgFields args)

  where
  opaqueField = TsRecordField
    (TsName $ "opaque_" <> printTsName name)
    { optional: false, readonly: true }
    TsTypeUniqueSymbol

  mkArgFields idx name' = TsRecordField
    (TsName ("arg" <> show idx))
    { optional: false, readonly: true }
    (TsTypeVar name')

filePathToModulePath :: TsFilePath -> TsModulePath
filePathToModulePath (TsFilePath x _) = TsModulePath x
