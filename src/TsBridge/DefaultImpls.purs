module TsBridge.DefaultImpls
  ( TypeVar(..)
  , class TsBridgeRecord
  , class TsBridgeRecordRL
  , class TsBridgeVariant
  , class TsBridgeVariantEncodedFlat
  , class TsBridgeVariantEncodedFlatRL
  , class TsBridgeVariantEncodedNested
  , class TsBridgeVariantEncodedNestedRL
  , class TsBridgeVariantRL
  , tsBridgeArray
  , tsBridgeBoolean
  , tsBridgeChar
  , tsBridgeEffect
  , tsBridgeEither
  , tsBridgeFn2
  , tsBridgeFunction
  , tsBridgeInt
  , tsBridgeMaybe
  , tsBridgeNewtype
  , tsBridgeNull
  , tsBridgeNullable
  , tsBridgeNumber
  , tsBridgeObject
  , tsBridgeOneOf
  , tsBridgeOpaqueType
  , tsBridgePromise
  , tsBridgeRecord
  , tsBridgeRecordRL
  , tsBridgeString
  , tsBridgeStringLit
  , tsBridgeTuple
  , tsBridgeTypeVar
  , tsBridgeUndefined
  , tsBridgeUnit
  , tsBridgeVariant
  , tsBridgeVariantEncodedFlat
  , tsBridgeVariantEncodedFlatRL
  , tsBridgeVariantEncodedNested
  , tsBridgeVariantEncodedNestedRL
  , tsBridgeVariantRL
  ) where

import Prelude

import Control.Monad.Writer (censor, listen, tell)
import Control.Promise (Promise)
import DTS (OSet(..))
import DTS as DTS
import Data.Array (mapWithIndex, (:))
import Data.Array as A
import Data.Either (Either)
import Data.Function.Uncurried (Fn2)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over)
import Data.Nullable (Nullable)
import Data.Set as Set
import Data.Set.Ordered as OSet
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Data.Variant.Encodings.Flat (VariantEncodedFlat)
import Data.Variant.Encodings.Nested (VariantEncodedNested)
import Effect (Effect)
import Foreign.Object (Object)
import Literals (StringLit)
import Literals.Null (Null)
import Literals.Undefined as Lit
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Record as R
import Safe.Coerce (coerce)
import TsBridge.Core (class TsBridgeBy, tsBridgeBy)
import TsBridge.Monad (Scope(..), TsBridgeAccum(..), TsBridgeM, getAccum)
import TsBridge.Types (AppError(..), mapErr, mkName, toTsName)
import Type.Proxy (Proxy(..))
import Untagged.Union (OneOf)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Represents a monomorphized type variable. E.g. a `Maybe a` can become a
-- | `Maybe (TypeVar "A")`. It's useful to create some type aliases for
-- | variables that are used often, like: `type A = TypeVar "A"`.
data TypeVar (s :: Symbol) = TypeVar

-------------------------------------------------------------------------------
-- tsBridge methods
-------------------------------------------------------------------------------

-- | `tsBridge` type class method implementation for type variables.
-- | This is needed because polymorphic values cannot be exported directly.
-- | They have to be monomorphized: E.g. something of type `Maybe a` needs to be
-- | typed `Maybe (Var "A")` to be exported.
tsBridgeTypeVar :: forall s. IsSymbol s => Proxy (TypeVar s) -> TsBridgeM DTS.TsType
tsBridgeTypeVar _ = do

  name <- mkName $ reflectSymbol (Proxy :: _ s)

  let
    scope = Scope
      { floating: coerce $ OSet.singleton $ toTsName name
      , fixed: OSet.empty
      }

  tell
    $ over TsBridgeAccum _ { scope = scope } mempty
  pure $ DTS.TsTypeVar $ toTsName name

-- | `tsBridge` type class method implementation for the `Number` type.
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#number)
-- | for details.
tsBridgeNumber :: Proxy Number -> TsBridgeM DTS.TsType
tsBridgeNumber _ = pure DTS.TsTypeNumber

-- | `tsBridge` type class method implementation for the `String` type
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#string)
-- | for details.
tsBridgeString :: Proxy String -> TsBridgeM DTS.TsType
tsBridgeString _ = pure DTS.TsTypeString

-- | `tsBridge` type class method implementation for the `Boolean` type
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#boolean)
-- | for details.
tsBridgeBoolean :: Proxy Boolean -> TsBridgeM DTS.TsType
tsBridgeBoolean _ = pure DTS.TsTypeBoolean

-- | `tsBridge` type class method implementation for the `Int` type
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#int)
-- | for details.
tsBridgeInt :: Proxy Int -> TsBridgeM DTS.TsType
tsBridgeInt = tsBridgeOpaqueType
  { moduleName: "Prim"
  , typeName: "Int"
  , typeArgs: []
  }

-- | `tsBridge` type class method implementation for the `Char` type
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#char)
-- | for details.
tsBridgeChar :: Proxy Char -> TsBridgeM DTS.TsType
tsBridgeChar = tsBridgeOpaqueType
  { moduleName: "Prim"
  , typeName: "Char"
  , typeArgs: []
  }

-- | `tsBridge` type class method implementation for the `Unit` type
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#unit)
-- | for details.
tsBridgeUnit :: Proxy Unit -> TsBridgeM DTS.TsType
tsBridgeUnit _ = pure DTS.TsTypeVoid

-- | `tsBridge` type class method implementation for the `Effect` type
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#number)
-- | for details.
tsBridgeEffect :: forall tok a. TsBridgeBy tok a => tok -> Proxy (Effect a) -> TsBridgeM DTS.TsType
tsBridgeEffect tok _ = censor mapAccum ado
  ret /\ TsBridgeAccum { scope: Scope scopeRet } <- listen $ tsBridgeBy tok (Proxy :: _ a)
  let
    newFixed = (scopeRet.fixed)
      <> scopeRet.floating

    removeQuant =
      DTS.mapQuantifier $ coerce $ OSet.filter (_ `OSet.notElem` newFixed)

  in
    DTS.TsTypeFunction (DTS.TsTypeArgsQuant $ coerce newFixed) []
      (removeQuant ret)
  where
  mapAccum = over TsBridgeAccum (\x -> x { scope = fixScope x.scope })

-- | `tsBridge` type class method implementation for the `Array` type
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#array)
-- | for details.
tsBridgeArray :: forall a tok. TsBridgeBy tok a => tok -> Proxy (Array a) -> TsBridgeM DTS.TsType
tsBridgeArray tok _ = DTS.TsTypeArray <$> tsBridgeBy tok (Proxy :: _ a)

tsBridgeObject :: forall tok a. TsBridgeBy tok a => tok -> Proxy (Object a) -> TsBridgeM DTS.TsType
tsBridgeObject tok _ = do
  x <- tsBridgeBy tok (Proxy :: _ a)
  pure $ DTS.TsTypeConstructor
    (DTS.TsQualName Nothing $ DTS.TsName "Record")
    (DTS.TsTypeArgs [ DTS.TsTypeString, x ])

-- | tsBridge type class method implementation for the `Tuple` type
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#tuple)
-- | for details.
tsBridgeTuple
  :: forall tok a b
   . TsBridgeBy tok a
  => TsBridgeBy tok b
  => tok
  -> Proxy (Tuple a b)
  -> TsBridgeM DTS.TsType
tsBridgeTuple tok =
  tsBridgeOpaqueType
    { moduleName: "Data.Tuple"
    , typeName: "Tuple"
    , typeArgs:
        [ "A" /\ tsBridgeBy tok (Proxy :: _ a)
        , "B" /\ tsBridgeBy tok (Proxy :: _ b)
        ]
    }

-- | `tsBridge` type class method implementation for the `Either` type
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#either)
-- | for details.
-- | 
tsBridgeEither
  :: forall tok a b
   . TsBridgeBy tok a
  => TsBridgeBy tok b
  => tok
  -> Proxy (Either a b)
  -> TsBridgeM DTS.TsType
tsBridgeEither tok =
  tsBridgeOpaqueType
    { moduleName: "Data.Either"
    , typeName: "Either"
    , typeArgs:
        [ "A" /\ tsBridgeBy tok (Proxy :: _ a)
        , "B" /\ tsBridgeBy tok (Proxy :: _ b)
        ]
    }

-- | `tsBridge` type class method implementation for the `Maybe` type
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#maybe)
-- | for details.
tsBridgeMaybe :: forall tok a. TsBridgeBy tok a => tok -> Proxy (Maybe a) -> TsBridgeM DTS.TsType
tsBridgeMaybe tok =
  tsBridgeOpaqueType
    { moduleName: "Data.Maybe"
    , typeName: "Maybe"
    , typeArgs:
        [ "A" /\ tsBridgeBy tok (Proxy :: _ a)
        ]
    }

-- | `tsBridge` type class method implementation for the `Promise` type.
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#promise)
-- | for details.
tsBridgePromise :: forall tok a. TsBridgeBy tok a => tok -> Proxy (Promise a) -> TsBridgeM DTS.TsType
tsBridgePromise tok _ = do
  x <- tsBridgeBy tok (Proxy :: _ a)
  pure $ DTS.TsTypeConstructor
    (DTS.TsQualName Nothing (DTS.TsName "Promise"))
    (DTS.TsTypeArgs [ x ])

-- | `tsBridge` type class method implementation for the `Nullable` type. 
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#number)
-- | for details.
tsBridgeNullable :: forall a tok. TsBridgeBy tok a => tok -> Proxy (Nullable a) -> TsBridgeM DTS.TsType
tsBridgeNullable tok _ = do
  x <- tsBridgeBy tok (Proxy :: _ a)
  pure $ DTS.TsTypeUnion
    [ DTS.TsTypeNull, x ]

-- | `tsBridge` type class method implementation for the `OneOf` type. 
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#oneof)
-- | for details.
tsBridgeOneOf
  :: forall a b tok
   . TsBridgeBy tok a
  => TsBridgeBy tok b
  => tok
  -> Proxy (OneOf a b)
  -> TsBridgeM DTS.TsType
tsBridgeOneOf tok _ = do
  x <- tsBridgeBy tok (Proxy :: _ a)
  y <- tsBridgeBy tok (Proxy :: _ b)

  pure $ DTS.TsTypeUnion [ x, y ]

-- | `tsBridge` type class method implementation for the `Undefined` type.
tsBridgeUndefined :: Proxy Lit.Undefined -> TsBridgeM DTS.TsType
tsBridgeUndefined _ = pure $ DTS.TsTypeVar (DTS.TsName "undefined")

tsBridgeNull :: Proxy Null -> TsBridgeM DTS.TsType
tsBridgeNull _ = pure DTS.TsTypeNull

-- | `tsBridge` type class method implementation for string literal types.
tsBridgeStringLit :: forall sym. IsSymbol sym => Proxy (StringLit sym) -> TsBridgeM DTS.TsType
tsBridgeStringLit _ = pure $ DTS.TsTypeTypelevelString $ reflectSymbol (Proxy :: _ sym)

-------------------------------------------------------------------------------
-- tsBridge methods / class VariantEncodedFlat 
-------------------------------------------------------------------------------

class TsBridgeVariantEncodedFlat :: Type -> Symbol -> Row Type -> Constraint
class TsBridgeVariantEncodedFlat tok symTag r where
  -- | `tsBridge` type class method implementation for the `TsBridgeVariantEncodedFlat` type
  -- |
  -- | See [this
  -- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#VariantEncodedFlat)
  -- | for details.
  tsBridgeVariantEncodedFlat :: tok -> Proxy (VariantEncodedFlat symTag r) -> TsBridgeM DTS.TsType

instance (RowToList r rl, TsBridgeVariantEncodedFlatRL tok symTag rl) => TsBridgeVariantEncodedFlat tok symTag r where
  tsBridgeVariantEncodedFlat tok _ = DTS.TsTypeUnion <$> tsBridgeVariantEncodedFlatRL tok (Proxy :: _ symTag) (Proxy :: _ rl)

-------------------------------------------------------------------------------
-- tsBridge methods / class TsBridgeVariantRL
-------------------------------------------------------------------------------

class TsBridgeVariantEncodedFlatRL :: Type -> Symbol -> RowList Type -> Constraint
class TsBridgeVariantEncodedFlatRL tok symTag rl where
  -- | `tsBridge` type class method implementation for the `TsBridgeVariantEncodedNested` type
  -- |
  -- | See [this
  -- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#VariantEncodedNested)
  -- | for details.
  tsBridgeVariantEncodedFlatRL :: tok -> Proxy symTag -> Proxy rl -> TsBridgeM (Array DTS.TsType)

instance TsBridgeVariantEncodedFlatRL tok symTag Nil where
  tsBridgeVariantEncodedFlatRL _ _ _ = pure []

instance
  ( TsBridgeBy tok a
  , TsBridgeVariantEncodedFlatRL tok symTag rl
  , IsSymbol s
  , IsSymbol symTag
  ) =>
  TsBridgeVariantEncodedFlatRL tok symTag (Cons s a rl) where
  tsBridgeVariantEncodedFlatRL tok prxSymTag _ =
    do
      x <- tsBridgeBy tok (Proxy :: _ a)
      xs <- tsBridgeVariantEncodedFlatRL tok prxSymTag (Proxy :: _ rl)
      pure $
        A.cons
          ( DTS.TsTypeIntersection
              [ DTS.TsTypeRecord
                  [ DTS.TsRecordField (reflectSymbol prxSymTag)
                      { readonly: true, optional: false }
                      (DTS.TsTypeTypelevelString $ reflectSymbol (Proxy :: _ s))
                  ]
              , x
              ]
          )
          xs

--------------------------------------------------------------------------------

-- | `tsBridge` type class method implementation for the `a -> b` (Function) type
-- |
-- | See [this
-- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#function)
-- | for details.
tsBridgeFunction
  :: forall tok a b
   . TsBridgeBy tok a
  => TsBridgeBy tok b
  => tok
  -> Proxy (a -> b)
  -> TsBridgeM DTS.TsType
tsBridgeFunction tok _ = censor mapAccum ado
  arg /\ TsBridgeAccum { scope: Scope scopeArg } <- listen $ tsBridgeBy tok (Proxy :: _ a)
  ret /\ TsBridgeAccum { scope: Scope scopeRet } <- listen $ tsBridgeBy tok (Proxy :: _ b)
  let
    newFixed = (OSet.intersect scopeArg.fixed scopeRet.fixed)
      <> scopeArg.floating
      <> scopeRet.floating

    removeQuant =
      DTS.mapQuantifier $ coerce $ OSet.filter (_ `OSet.notElem` newFixed)

  in
    DTS.TsTypeFunction (DTS.TsTypeArgsQuant $ coerce newFixed)
      [ DTS.TsFnArg (DTS.TsName "_") (removeQuant arg)
      ]
      (removeQuant ret)
  where
  mapAccum = over TsBridgeAccum (\x -> x { scope = fixScope x.scope })

tsBridgeFn2
  :: forall tok a1 a2 b
   . TsBridgeBy tok a1
  => TsBridgeBy tok a2
  => TsBridgeBy tok b
  => tok
  -> Proxy (Fn2 a1 a2 b)
  -> TsBridgeM DTS.TsType
tsBridgeFn2 tok _ = censor mapAccum ado
  arg1 /\ TsBridgeAccum { scope: Scope scopeArg1 } <- listen $ tsBridgeBy tok (Proxy :: _ a1)
  arg2 /\ TsBridgeAccum { scope: Scope scopeArg2 } <- listen $ tsBridgeBy tok (Proxy :: _ a2)
  ret /\ TsBridgeAccum { scope: Scope scopeRet } <- listen $ tsBridgeBy tok (Proxy :: _ b)
  let
    newFixed =
      ( scopeRet.fixed
          # OSet.intersect scopeArg1.fixed
          # OSet.intersect scopeArg2.fixed
      )
        <> scopeArg1.floating
        <> scopeArg2.floating
        <> scopeRet.floating

    removeQuant =
      DTS.mapQuantifier $ coerce $ OSet.filter (_ `OSet.notElem` newFixed)

  in
    DTS.TsTypeFunction (DTS.TsTypeArgsQuant $ coerce newFixed)
      [ DTS.TsFnArg (DTS.TsName "arg1") (removeQuant arg1)
      , DTS.TsFnArg (DTS.TsName "arg2") (removeQuant arg2)
      ]
      (removeQuant ret)
  where
  mapAccum = over TsBridgeAccum (\x -> x { scope = fixScope x.scope })

-- | `tsBridge` type class method implementation for opaque types
tsBridgeOpaqueType :: forall a. { moduleName :: String, typeName :: String, typeArgs :: Array (String /\ TsBridgeM DTS.TsType) } -> Proxy a -> TsBridgeM DTS.TsType
tsBridgeOpaqueType { moduleName, typeName, typeArgs } _ =
  mapErr (AtType typeName)
    do
      argNames <- typeArgs <#> fst # traverse mkName
      targs <- typeArgs <#> snd # sequence
      name <- mkName typeName

      let
        filePath = DTS.TsFilePath (moduleName <> "/index.d.ts")
        importPath = DTS.TsImportPath ("../" <> moduleName)

        typeDefs =
          [ DTS.TsModuleFile
              filePath
              ( DTS.TsModule
                  [ mkBrandedTypeDecl (toTsName name) (coerce $ OSet.fromFoldable $ toTsName <$> argNames) Nothing
                  ]
              )
          ]

      tell
        $ TsBridgeAccum
        $ R.union mempty { typeDefs }

      pure
        $ DTS.TsTypeConstructor (DTS.TsQualName (Just importPath) (toTsName name)) (DTS.TsTypeArgs targs)

-- | `tsBridge` type class method implementation for newtypes
tsBridgeNewtype
  :: forall tok a t
   . Newtype a t
  => TsBridgeBy tok t
  => tok
  -> { moduleName :: String, typeName :: String, typeArgs :: Array (String /\ TsBridgeM DTS.TsType) }
  -> Proxy a
  -> TsBridgeM DTS.TsType
tsBridgeNewtype tok { moduleName, typeName, typeArgs } _ =
  mapErr (AtType typeName)
    do
      let
        targNames = map fst typeArgs
        targs = map snd typeArgs
        filePath = DTS.TsFilePath (moduleName <> "/index.d.ts")
        filePathRef = DTS.TsImportPath ("../" <> moduleName)

      name <- mkName typeName
      args <- sequence targs

      TsBridgeAccum accum <- getAccum

      unless (Set.member { moduleName, typeName } accum.registeredTypes) do

        tell
          $ TsBridgeAccum
          $ R.union mempty { registeredTypes: Set.singleton { moduleName, typeName } }

        x <- tsBridgeBy tok (Proxy :: _ t)

        args' <- OSet.fromFoldable <$> map toTsName <$> traverse mkName targNames

        let
          removeQuant =
            DTS.mapQuantifier $ coerce $ OSet.filter (_ `OSet.notElem` args')

        let
          typeDefs =
            [ DTS.TsModuleFile
                filePath
                ( DTS.TsModule
                    [ DTS.TsDeclTypeDef (toTsName name) DTS.Public (coerce args') (removeQuant x)
                    ]
                )
            ]

        tell
          $ TsBridgeAccum
          $ R.union mempty { typeDefs, registeredTypes: Set.singleton { moduleName, typeName } }

      pure
        $ DTS.TsTypeConstructor (DTS.TsQualName (Just filePathRef) (toTsName name)) (DTS.TsTypeArgs args)

-------------------------------------------------------------------------------
-- tsBridge methods / class TsBridgeRecord
-------------------------------------------------------------------------------

class TsBridgeRecord :: Type -> Row Type -> Constraint
class TsBridgeRecord tok r where
  -- | `tsBridge` type class method implementation for the Record type
  -- |
  -- | See [this
  -- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#record)
  -- | for details.
  tsBridgeRecord :: tok -> Proxy (Record r) -> TsBridgeM DTS.TsType

instance (RowToList r rl, TsBridgeRecordRL tok rl) => TsBridgeRecord tok r where
  tsBridgeRecord tok _ = DTS.TsTypeRecord <$> tsBridgeRecordRL tok (Proxy :: _ rl)

-------------------------------------------------------------------------------
-- tsBridge methods / class TsBridgeRecordRL
-------------------------------------------------------------------------------

class TsBridgeRecordRL :: Type -> RowList Type -> Constraint
class TsBridgeRecordRL tok rl where
  tsBridgeRecordRL :: tok -> Proxy rl -> TsBridgeM (Array DTS.TsRecordField)

instance TsBridgeRecordRL tok Nil where
  tsBridgeRecordRL _ _ = pure []

instance
  ( TsBridgeBy tok t
  , TsBridgeRecordRL tok rl
  , IsSymbol s
  ) =>
  TsBridgeRecordRL tok (Cons s t rl) where
  tsBridgeRecordRL tok _ = do
    x <- tsBridgeBy tok (Proxy :: _ t)
    xs <- tsBridgeRecordRL tok (Proxy :: _ rl)
    let k = reflectSymbol (Proxy :: _ s)
    pure $
      A.cons (DTS.TsRecordField k { optional: false, readonly: true } x) xs

-------------------------------------------------------------------------------
-- tsBridge methods / class TsBridgeVariant
-------------------------------------------------------------------------------

class TsBridgeVariant :: Type -> Row Type -> Constraint
class TsBridgeVariant tok r where
  -- | `tsBridge` type class method implementation for the Variant type
  -- |
  -- | See [this
  -- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#variant)
  -- | for details.
  tsBridgeVariant :: tok -> Proxy (Variant r) -> TsBridgeM DTS.TsType

instance (RowToList r rl, TsBridgeVariantRL tok rl) => TsBridgeVariant tok r where
  tsBridgeVariant tok _ = DTS.TsTypeUnion <$> tsBridgeVariantRL tok (Proxy :: _ rl)

-------------------------------------------------------------------------------
-- tsBridge methods / class TsBridgeVariantRL
-------------------------------------------------------------------------------

class TsBridgeVariantRL :: Type -> RowList Type -> Constraint
class TsBridgeVariantRL tok rl where
  tsBridgeVariantRL :: tok -> Proxy rl -> TsBridgeM (Array DTS.TsType)

instance TsBridgeVariantRL tok Nil where
  tsBridgeVariantRL _ _ = pure []

instance
  ( TsBridgeBy tok t
  , TsBridgeVariantRL tok rl
  , IsSymbol s
  ) =>
  TsBridgeVariantRL tok (Cons s t rl) where
  tsBridgeVariantRL tok _ =
    do
      x <- tsBridgeBy tok (Proxy :: _ t)
      xs <- tsBridgeVariantRL tok (Proxy :: _ rl)
      pure $
        A.cons
          ( DTS.TsTypeRecord
              [ DTS.TsRecordField "type"
                  { readonly: true, optional: false }
                  (DTS.TsTypeTypelevelString $ reflectSymbol (Proxy :: _ s))
              , DTS.TsRecordField "value"
                  { readonly: true, optional: false }
                  x
              ]
          )
          xs

-------------------------------------------------------------------------------
-- tsBridge methods / class TsBridgeVariantEncodedNested
-------------------------------------------------------------------------------

class TsBridgeVariantEncodedNested :: Type -> Symbol -> Symbol -> Row Type -> Constraint
class TsBridgeVariantEncodedNested tok symTag symVal r where
  -- | `tsBridge` type class method implementation for the VariantEncodedNested type
  -- |
  -- | See [this
  -- | reference](https://github.com/thought2/purescript-ts-bridge/blob/main/docs/type-comparison.md#VariantEncodedNested)
  -- | for details.
  tsBridgeVariantEncodedNested :: tok -> Proxy (VariantEncodedNested symTag symVal r) -> TsBridgeM DTS.TsType

instance (RowToList r rl, TsBridgeVariantEncodedNestedRL tok symTag symVal rl) => TsBridgeVariantEncodedNested tok symTag symVal r where
  tsBridgeVariantEncodedNested tok _ = DTS.TsTypeUnion <$> tsBridgeVariantEncodedNestedRL tok (Proxy :: _ symTag) (Proxy :: _ symVal) (Proxy :: _ rl)

-------------------------------------------------------------------------------
-- tsBridge methods / class TsBridgeVariantEncodedNestedRL
-------------------------------------------------------------------------------

class TsBridgeVariantEncodedNestedRL :: Type -> Symbol -> Symbol -> RowList Type -> Constraint
class TsBridgeVariantEncodedNestedRL tok symTag symVal rl where
  tsBridgeVariantEncodedNestedRL :: tok -> Proxy symTag -> Proxy symVal -> Proxy rl -> TsBridgeM (Array DTS.TsType)

instance TsBridgeVariantEncodedNestedRL tok symTag symVal Nil where
  tsBridgeVariantEncodedNestedRL _ _ _ _ = pure []

instance
  ( TsBridgeBy tok t
  , TsBridgeVariantEncodedNestedRL tok symTag symVal rl
  , IsSymbol s
  , IsSymbol symTag
  , IsSymbol symVal
  ) =>
  TsBridgeVariantEncodedNestedRL tok symTag symVal (Cons s t rl) where
  tsBridgeVariantEncodedNestedRL tok prxSymTag prxSymVal _ =
    do
      x <- tsBridgeBy tok (Proxy :: _ t)
      xs <- tsBridgeVariantEncodedNestedRL tok prxSymTag prxSymVal (Proxy :: _ rl)
      pure $
        A.cons
          ( DTS.TsTypeRecord
              [ DTS.TsRecordField (reflectSymbol prxSymTag)
                  { readonly: true, optional: false }
                  (DTS.TsTypeTypelevelString $ reflectSymbol (Proxy :: _ s))
              , DTS.TsRecordField (reflectSymbol prxSymVal)
                  { readonly: true, optional: false }
                  x
              ]
          )
          xs

-------------------------------------------------------------------------------
-- Util
-------------------------------------------------------------------------------

fixScope :: Scope -> Scope
fixScope (Scope { fixed, floating }) = Scope
  { floating: OSet.empty
  , fixed: fixed <> floating
  }

mkBrandedTypeDecl :: DTS.TsName -> OSet DTS.TsName -> Maybe DTS.TsType -> DTS.TsDeclaration
mkBrandedTypeDecl name args type_ = DTS.TsDeclTypeDef name DTS.Public (coerce args)
  $ maybeWithType
  $
    DTS.TsTypeRecord
      (opaqueField : (mapWithIndex mkArgFields $ OSet.toUnfoldable $ coerce args))

  where
  maybeWithType = case type_ of
    Nothing -> identity
    Just t -> \x -> DTS.TsTypeIntersection [ x, t ]

  opaqueField = DTS.TsRecordField
    "__brand"
    { optional: false, readonly: true }
    DTS.TsTypeUniqueSymbol

  mkArgFields idx name' = DTS.TsRecordField
    ("__arg" <> show (idx + 1))
    { optional: false, readonly: true }
    (DTS.TsTypeVar name')
