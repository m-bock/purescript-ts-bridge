module TsBridge.Class
  ( TsBridge(..)
  , TsBridgeAccum
  , class GenRecord
  , class ToTsBridge
  , genRecord
  , runTsBridge
  , toTsBridge
  )
  where

import Prelude

import Control.Monad.Writer (class MonadTell, class MonadWriter, Writer, runWriter, tell)
import Data.Array as A
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Undefined (undefined)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import TsBridge.DTS (TsDeclaration(..), TsFilePath(..), TsImport(..), TsName(..), TsQualName(..), TsType(..))
import Type.Proxy (Proxy(..))

-------------------------------------------------------------------------------
-- Types / TsBridge
-------------------------------------------------------------------------------

newtype TsBridge a = TsBridge (Writer TsBridgeAccum a)

type TsBridgeAccum =
  { typeDefs :: Set (TsFilePath /\ TsDeclaration)
  , imports :: Set TsImport
  }

derive newtype instance MonadTell TsBridgeAccum TsBridge
derive newtype instance Monad TsBridge
derive newtype instance Bind TsBridge
derive newtype instance Functor TsBridge
derive newtype instance Apply TsBridge
derive newtype instance Applicative TsBridge

runTsBridge :: forall a. TsBridge a -> a /\ TsBridgeAccum
runTsBridge (TsBridge ma) = runWriter ma

-------------------------------------------------------------------------------
-- Class / ToTsBridge
-------------------------------------------------------------------------------

class ToTsBridge a where
  toTsBridge :: a -> TsBridge TsType

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
  toTsBridge _ = TsTypeRecord <$> genRecord (Proxy :: _ rl)

instance (ToTsBridge a, ToTsBridge b) => ToTsBridge (a -> b) where
  toTsBridge _ = ado
    arg <- toTsBridge (Proxy :: _ a)
    ret <- toTsBridge (Proxy :: _ b)
    in
      TsTypeFunction Set.empty
        [ TsName "_" /\ arg ]
        ret

-------------------------------------------------------------------------------
-- Class / ToTsBridge / Standard Types
-------------------------------------------------------------------------------

instance ToTsBridge a => ToTsBridge (Maybe a) where
  toTsBridge _ = ado
    x <- toTsBridge (Proxy :: _ a)
    tell
      { typeDefs: Set.singleton $
          TsFilePath "Data_Maybe/index" /\
            TsDeclTypeDef (TsName "Maybe") [] (TsTypeRecord [])
      , imports: Set.singleton $
          TsImport (TsName "Data_Maybe") (TsFilePath "Data_Maybe/index")
      }
    in TsTypeConstructor (TsQualName (Just "Data_Maybe") "Maybe") [ x ]

-------------------------------------------------------------------------------
-- Class / GenRecord
-------------------------------------------------------------------------------

class GenRecord :: RowList Type -> Constraint
class GenRecord rl where
  genRecord :: Proxy rl -> TsBridge (Array (TsName /\ TsType))

instance GenRecord Nil where
  genRecord _ = pure []

instance (GenRecord rl, ToTsBridge t, IsSymbol s) => GenRecord (Cons s t rl) where
  genRecord _ = ado
    x <- toTsBridge (Proxy :: _ t)
    xs <- genRecord (Proxy :: _ rl)
    in
      A.cons (TsName (reflectSymbol (Proxy :: _ s)) /\ x) xs
