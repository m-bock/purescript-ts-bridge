module TsBridge.TsBridgeM
  ( TsBridgeAccum
  , TsBridgeM(..)
  , opaqueType
  , runTsBridge
  ) where

import Prelude

import Control.Monad.Writer (class MonadTell, Writer, runWriter, tell)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (sequence)
import Data.Tuple.Nested (type (/\))
import TsBridge.DTS (TsDeclaration(..), TsFilePath(..), TsImport(..), TsModule(..), TsModuleFile(..), TsModulePath(..), TsName(..), TsQualName(..), TsRecord(..), TsType(..), TsTypeArgs(..))

-------------------------------------------------------------------------------
-- Types / TsBridge
-------------------------------------------------------------------------------

newtype TsBridgeM a = TsBridgeM (Writer TsBridgeAccum a)

type TsBridgeAccum = 
  { typeDefs :: Array TsModuleFile
  , imports :: Set TsImport
  , floating :: Set TsName
  , quantified :: Set TsName
  }


derive newtype instance MonadTell TsBridgeAccum TsBridgeM
derive newtype instance Monad TsBridgeM
derive newtype instance Bind TsBridgeM
derive newtype instance Functor TsBridgeM
derive newtype instance Apply TsBridgeM
derive newtype instance Applicative TsBridgeM

runTsBridge :: forall a. TsBridgeM a -> a /\ TsBridgeAccum
runTsBridge (TsBridgeM ma) = runWriter ma

-------------------------------------------------------------------------------
-- Util
-------------------------------------------------------------------------------

opaqueType :: String -> String -> Array (TsBridgeM TsType) -> TsBridgeM TsType
opaqueType _ _ xs = ado
  xs' <- sequence xs
  tell
    { typeDefs:
        [ TsModuleFile
            (TsFilePath "Data_Maybe/index.d.ts")
            ( TsModule Set.empty
                [ TsDeclTypeDef (TsName "Maybe") []
                    (TsTypeRecord (TsRecord []))
                ]
            )
        ]
    , imports: Set.singleton $
        TsImport
          (TsName "Data_Maybe")
          (TsModulePath "Data_Maybe/index")
    , floating: Set.empty
    , quantified: Set.empty
    }
  in TsTypeConstructor (TsQualName (Just "Data_Maybe") "Maybe") (TsTypeArgs xs')