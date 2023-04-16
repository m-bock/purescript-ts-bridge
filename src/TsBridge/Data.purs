module TsBridge.Data where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (isRight)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Foreign as F
import Literals (StringLit)
import Type.Proxy (Proxy(..))
import Untagged.Castable (class Castable)
import Untagged.TypeCheck (class HasRuntimeType, hasRuntimeType)
import Untagged.Union (type (|+|))
import Untagged.Union as TU

data My = A Int | B String

type My' =
  { kind :: StringLit "A"
  , value :: Int
  } |+|
    { kind :: StringLit "B"
    , value :: Int
    }

-- type T1 = Int |+| String |+| Boolean

-- newtype NativeSym :: Symbol -> Type
-- newtype NativeSym sym = NativeSym String

-- mkNativeSym :: forall sym. IsSymbol sym => Proxy sym -> NativeSym sym
-- mkNativeSym prx = NativeSym (reflectSymbol prx)

-- instance Castable (NativeSym sym) String

-- instance HasRuntimeType (NativeSym sym) where
--   hasRuntimeType _ fo =
--     F.readString fo
--       # runExcept
--       # isRight

-- type T2 = NativeSym "one" |+| NativeSym "two"

-- v1 :: T2
-- v1 = TU.asOneOf $ mkNativeSym (Proxy :: _ "one")

-- v2 = TU.

-- foreign import data MkCases :: Row Fields -> Cases

-- foreign import data MkFields :: Row Type -> Fields

-- foreign import data MkTag :: Symbol -> Tag

-- foreign import data Fields :: Type

-- foreign import data Cases :: Type

-- foreign import data Tag :: Type

-- foreign import data CaseValue :: Type

-- foreign import data MkCaseValue :: Symbol -> CaseValue

-- foreign import data MkCaseValue :: Symbol -> CaseValue

-- newtype TaggedUnion :: Tag -> Value -> Cases -> Type
-- newtype TaggedUnion tag val cases = FlatTaggedUnion Unit

-- type T = FlatTaggedUnion
--   (MkTag "kind")
--   ( MkCases
--       ( "caseOne" ::
--           MkFields
--             ( "fieldA" :: Int
--             )
--       , "caseTwo" ::
--           MkFields
--             ( "fieldB" :: String
--             , "fieldC" :: Boolean
--             )
--       )
--   )