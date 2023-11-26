module NTuple
  ( NTuple
  , cons
  , empty
  , snoc
  ) where

import Type.Data.List (type (:>), List', Nil')

data NTuple :: forall k. List' k -> Type
data NTuple a

foreign import empty :: NTuple Nil'

foreign import cons :: forall a as. a -> NTuple as -> NTuple (a :> as)

foreign import snoc :: forall a as. NTuple as -> a -> NTuple (a :> as)
