module TsBridge.Monad
  ( TsBridgeAccum(..)
  , TsBridgeM(..)
  , Wrap(..)
  , opaqueType
  , runTsBridgeM
  ) where

import Prelude

import Control.Monad.Writer (class MonadTell, class MonadWriter, Writer, runWriter, tell)
import Data.Array (mapWithIndex, (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.Set.Ordered (OSet)
import Data.Set.Ordered as OSet
import Data.Traversable (sequence)
import Data.Tuple.Nested (type (/\))
import Record as R
import Safe.Coerce (coerce)
import TsBridge.DTS (TsDeclaration(..), TsFilePath(..), TsImport(..), TsModule(..), TsModuleAlias, TsModuleFile(..), TsModulePath(..), TsName(..), TsQualName(..), TsRecordField(..), TsType(..), TsTypeArgs(..))
import TsBridge.Print (printTsName)
import TsBridge.DTS as TsBridge.DTS

-------------------------------------------------------------------------------
-- Types / TsBridge
-------------------------------------------------------------------------------

newtype TsBridgeM a = TsBridgeM (Writer TsBridgeAccum a)

newtype TsBridgeAccum = TsBridgeAccum
  { typeDefs :: Array TsModuleFile
  , imports :: Set TsImport
  , scope :: Wrap (OSet TsName)
  }

runTsBridgeM :: forall a. TsBridgeM a -> a /\ TsBridgeAccum
runTsBridgeM (TsBridgeM ma) = runWriter ma

-------------------------------------------------------------------------------
-- Util
-------------------------------------------------------------------------------

opaqueType :: TsFilePath -> TsModuleAlias -> TsName -> OSet TsName -> Array (TsBridgeM TsType) -> TsBridgeM TsType
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
    $ TsBridgeAccum
    $ R.union mempty { typeDefs, imports }

  pure
    $ TsTypeConstructor (TsQualName (Just moduleAlias) name) (TsTypeArgs args)

mkOpaqueTypeDecl :: TsName -> OSet TsName -> TsDeclaration
mkOpaqueTypeDecl name args = TsDeclTypeDef name (coerce args) $
  TsTypeRecord
    (opaqueField : (mapWithIndex mkArgFields $ OSet.toUnfoldable args))

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

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

derive instance Newtype TsBridgeAccum _

derive newtype instance Monoid TsBridgeAccum

derive newtype instance Semigroup TsBridgeAccum

derive newtype instance MonadTell TsBridgeAccum TsBridgeM

derive newtype instance MonadWriter TsBridgeAccum TsBridgeM

derive newtype instance Monad TsBridgeM

derive newtype instance Bind TsBridgeM

derive newtype instance Functor TsBridgeM

derive newtype instance Apply TsBridgeM

derive newtype instance Applicative TsBridgeM

-------------------------------------------------------------------------------
-- Wrap
-------------------------------------------------------------------------------

newtype Wrap a = Wrap a

derive instance Newtype (Wrap a) _

derive newtype instance Eq a => Semigroup (Wrap (OSet a))
instance Eq a => Monoid (Wrap (OSet a)) where
  mempty = Wrap $ OSet.empty