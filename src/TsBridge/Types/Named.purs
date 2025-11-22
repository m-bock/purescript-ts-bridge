module TsBridge.Types.Named where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Type.Prelude (Proxy(..))

newtype Named :: Symbol -> Type -> Type
newtype Named sym a = Named a

derive instance Newtype (Named sym a) _

class GetName :: Type -> Constraint
class GetName a where
  getName :: Proxy a -> Maybe String

instance (IsSymbol sym) => GetName (Named sym a) where
  getName _ = Just $ reflectSymbol (Proxy :: _ sym)

else instance GetName a where
  getName _ = Nothing

newtype QualNamed (moduleName :: Symbol) (typeName :: Symbol) (a :: Type) = QualNamed a

derive instance Newtype (QualNamed moduleName typeName a) _