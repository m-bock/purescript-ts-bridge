module TsBridge.Class
  ( TsBridgeM(..)
  , TsBridgeAccum
  , class GenRecord
  , class ToTsBridge
  , genRecord
  , runTsBridge
  , toTsBridge
  ) where

import Prelude

import Control.Monad.Writer (class MonadTell, Writer, runWriter, tell)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (sequence)
import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Undefined (undefined)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import TsBridge.DTS (TsDeclaration(..), TsFilePath(..), TsFnArg(..), TsImport(..), TsModule(..), TsModuleFile(..), TsModulePath(..), TsName(..), TsQualName(..), TsRecord(..), TsRecordField(..), TsType(..), TsTypeArgs(..), TsTypeArgsQuant(..))
import Type.Proxy (Proxy(..))

-------------------------------------------------------------------------------
-- Types / TsBridge
-------------------------------------------------------------------------------

newtype TsBridgeM a = TsBridge (Writer TsBridgeAccum a)

type TsBridgeAccum =
  { typeDefs :: Array TsModuleFile
  , imports :: Set TsImport
  }

derive newtype instance MonadTell TsBridgeAccum TsBridgeM
derive newtype instance Monad TsBridgeM
derive newtype instance Bind TsBridgeM
derive newtype instance Functor TsBridgeM
derive newtype instance Apply TsBridgeM
derive newtype instance Applicative TsBridgeM

runTsBridge :: forall a. TsBridgeM a -> a /\ TsBridgeAccum
runTsBridge (TsBridge ma) = runWriter ma

-------------------------------------------------------------------------------
-- Class / ToTsBridge
-------------------------------------------------------------------------------

class ToTsBridge a where
  toTsBridge :: a -> TsBridgeM TsType

-------------------------------------------------------------------------------
-- Class / ToTsBridge / Proxy
-------------------------------------------------------------------------------

instance ToTsBridge a => ToTsBridge (Proxy a) where
  toTsBridge _ = toTsBridge (undefined :: a)

-------------------------------------------------------------------------------
-- Class / ToTsBridge / Primitives
-------------------------------------------------------------------------------

instance ToTsBridge Number where
  toTsBridge _ = pure TsTypeNumber

instance ToTsBridge String where
  toTsBridge _ = pure TsTypeString

instance ToTsBridge Boolean where
  toTsBridge _ = pure TsTypeBoolean

instance ToTsBridge a => ToTsBridge (Array a) where
  toTsBridge _ = TsTypeArray <$> toTsBridge (Proxy :: _ a)

instance (RowToList r rl, GenRecord rl) => ToTsBridge (Record r) where
  toTsBridge _ = TsTypeRecord <<< TsRecord <$> genRecord (Proxy :: _ rl)

instance (ToTsBridge a, ToTsBridge b) => ToTsBridge (a -> b) where
  toTsBridge _ = ado
    arg <- toTsBridge (Proxy :: _ a)
    ret <- toTsBridge (Proxy :: _ b)
    in
      TsTypeFunction (TsTypeArgsQuant Set.empty)
        [ TsFnArg (TsName "_") arg ]
        ret

-------------------------------------------------------------------------------
-- Class / ToTsBridge / Standard Types
-------------------------------------------------------------------------------

instance ToTsBridge a => ToTsBridge (Maybe a) where
  toTsBridge _ = opaqueType "Data.Maybe" "Maybe" [ toTsBridge (Proxy :: _ a) ]

-------------------------------------------------------------------------------
-- Class / GenRecord
-------------------------------------------------------------------------------

class GenRecord :: RowList Type -> Constraint
class GenRecord rl where
  genRecord :: Proxy rl -> TsBridgeM (Array TsRecordField)

instance GenRecord Nil where
  genRecord _ = pure []

instance (GenRecord rl, ToTsBridge t, IsSymbol s) => GenRecord (Cons s t rl) where
  genRecord _ = ado
    x <- toTsBridge (Proxy :: _ t)
    xs <- genRecord (Proxy :: _ rl)
    let k = TsName $ reflectSymbol (Proxy :: _ s)
    in
      A.cons (TsRecordField k { optional: false, readonly: true } x) xs

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
    }
  in TsTypeConstructor (TsQualName (Just "Data_Maybe") "Maybe") (TsTypeArgs xs')