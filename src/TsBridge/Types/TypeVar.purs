module TsBridge.Types.TypeVar where

import Prelude

-- | Represents a monomorphized type variable. E.g. a `Maybe a` can become a
-- | `Maybe (TypeVar "A")`. It's useful to create some type aliases for
-- | variables that are used often, like: `type A = TypeVar "A"`.
data TypeVar (s :: Symbol) = TypeVar

type A = TypeVar "A"

type B = TypeVar "B"

type C = TypeVar "C"

type D = TypeVar "D"

type E = TypeVar "E"

type F = TypeVar "F"

type G = TypeVar "G"

type H = TypeVar "H"

type I = TypeVar "I"

type J = TypeVar "J"

type K = TypeVar "K"

type L = TypeVar "L"

type M = TypeVar "M"

type N = TypeVar "N"

type O = TypeVar "O"

type P = TypeVar "P"

type Q = TypeVar "Q"

type R = TypeVar "R"

type S = TypeVar "S"

type T = TypeVar "T"

type U = TypeVar "U"

type V = TypeVar "V"

type W = TypeVar "W"

type X = TypeVar "X"

type Y = TypeVar "Y"

type Z = TypeVar "Z"