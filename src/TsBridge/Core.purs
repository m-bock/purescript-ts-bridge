module TsBridge.Core
  ( class TsBridgeBy
  , class TsValues
  , class TsValuesRL
  , tsBridgeBy
  , tsModuleFile
  , tsOpaqueType
  , tsProgram
  , tsTypeAlias
  , tsTypeAliasFromValue
  , tsValue
  , tsValues
  , tsValuesRL
  )
  where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Writer (censor, listens, tell)
import DTS (TsDeclaration(..))
import DTS as DTS
import Data.Array as A
import Data.Array as Arr
import Data.Array as Array
import Data.Either (Either)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Set as Set
import Data.Set.Ordered as OSet
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (for_, sequence)
import Data.Tuple.Nested ((/\))
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Safe.Coerce (coerce)
import TsBridge.Monad (Scope(..), TsBridgeAccum(..), TsBridgeM, runTsBridgeM)
import TsBridge.Types (AppError(..), mapErr, mkName, mkPursModuleName, toTsName)
import Type.Proxy (Proxy(..))

-- | Type Class that is used by the type generator to recursively traverse
-- | types.
-- | Instances for the specific types will be defined on the user's side with a
-- | typeclass like this:
-- | ```
-- | class TsBridge a where
-- |   tsBridge :: a -> StandaloneTsType
-- | ```
-- | Then the internal type class is forwarded to the
-- | one of the user. For this you need to define a token data type and an
-- | instance like this:
-- | ```
-- | data Tok = Tok
-- |
-- | instance TsBridge a => TsBridgeBy Tok a where
-- |   tsBridgeBy _ = tsBridge
-- | ```
-- | The token will then be passed to all generic functions of the library.

class TsBridgeBy :: Type -> Type -> Constraint
class TsBridgeBy tok a where
  tsBridgeBy :: tok -> Proxy a -> TsBridgeM DTS.TsType

tsModuleFile :: String -> Array (TsBridgeM (Array DTS.TsDeclaration)) -> Either AppError (Array DTS.TsModuleFile)
tsModuleFile n xs =
  mapErr (AtModule n)
    do
      _ <- mkPursModuleName n

      (xs' /\ TsBridgeAccum { typeDefs }) <- runTsBridgeM $ join <$> sequence xs

      let
        names = Arr.mapMaybe declToName xs'
        duplicate = Arr.head $ Arr.difference names (Arr.nub names)

      for_ duplicate (throwError <<< ErrDuplicateIdentifier)

      pure (typeDefs <> [ DTS.TsModuleFile (DTS.TsFilePath (n <> "/index.d.ts")) (DTS.TsModule xs') ])

declToName :: DTS.TsDeclaration -> Maybe DTS.TsName
declToName = case _ of
  TsDeclTypeDef name _ _ _ -> Just name
  TsDeclValueDef name _ _ -> Just name
  TsDeclComments _ -> Nothing

mergeModules :: Array DTS.TsModuleFile -> DTS.TsProgram
mergeModules xs =
  xs
    <#> (\(DTS.TsModuleFile mp m) -> mp /\ m)
    # Map.fromFoldableWith mergeModule
    # DTS.TsProgram

mergeModule :: DTS.TsModule -> DTS.TsModule -> DTS.TsModule
mergeModule (DTS.TsModule ds1) (DTS.TsModule ds2) =
  DTS.TsModule
    (Array.nub (ds1 <> ds2))

tsProgram :: Array (Either AppError (Array DTS.TsModuleFile)) -> Either AppError DTS.TsProgram
tsProgram xs =
  xs # sequence <#> join >>> mergeModules

-- | For rare cases where you want to export a type alias. References to this type
-- | alias will be fully resolved in the generated code. So it is more practical
-- | to use a newtype instead, which can be references by name.
tsTypeAlias :: forall tok a. TsBridgeBy tok a => tok -> String -> Proxy a -> TsBridgeM (Array DTS.TsDeclaration)
tsTypeAlias tok aliasName x = ado
  x /\ scope <- listens (un TsBridgeAccum >>> _.scope >>> un Scope) t
  name <- mkName aliasName
  in [ DTS.TsDeclTypeDef (toTsName name) DTS.Public (coerce scope.floating) x ]
  where
  t = tsBridgeBy tok x

tsTypeAliasFromValue :: forall tok a. TsBridgeBy tok a => tok -> String -> a -> TsBridgeM (Array DTS.TsDeclaration)
tsTypeAliasFromValue tok aliasName _ = tsTypeAlias tok aliasName (Proxy :: _ a)

-- | For rare cases where you want to manually export an opaque type. Once you export a
-- | value that contains a reference to this type, the type will be generated
-- | and exported automatically. Thus in most cases you don't need this.
tsOpaqueType :: forall tok a. TsBridgeBy tok a => tok -> Proxy a -> TsBridgeM (Array DTS.TsDeclaration)
tsOpaqueType tok x = do
  _ /\ modules <- listens (un TsBridgeAccum >>> _.typeDefs) $ tsBridgeBy tok x
  case A.uncons modules of
    Just { head: (DTS.TsModuleFile _ (DTS.TsModule decls)), tail: [] } -> do
      tell mempty
      pure decls
    _ -> pure []

-- | Exports a single PureScript value to TypeScript. `tsValues` may be better choice. 
tsValue :: forall tok a. TsBridgeBy tok a => tok -> String -> a -> TsBridgeM (Array DTS.TsDeclaration)
tsValue tok n _ = tsValue' tok n (Proxy :: _ a)

tsValue' :: forall tok a. TsBridgeBy tok a => tok -> String -> Proxy a -> TsBridgeM (Array DTS.TsDeclaration)
tsValue' tok n _ =
  censor (\(TsBridgeAccum acc) -> TsBridgeAccum acc { scope = mempty })
    $
      mapErr (AtValue n)
        do
          let t = tsBridgeBy tok (Proxy :: _ a)
          x /\ scope <- listens (un TsBridgeAccum >>> _.scope >>> un Scope) t

          name <- mkName n

          when (OSet.length scope.floating /= 0)
            ( throwError
                $ ErrUnquantifiedTypeVariables
                $ (Set.fromFoldable :: Array _ -> _)
                $ OSet.toUnfoldable scope.floating
            )

          pure [ DTS.TsDeclValueDef (toTsName name) DTS.Public x ]

--------------------------------------------------------------------------------
-- class TsValues
--------------------------------------------------------------------------------

class TsValues tok r where
  -- | Useful for declaring multiple PureScript values to be used by TypeScript.
  -- | Through record punning the risk of exporting them with wrong names can be eliminated.  
  -- | ```tsValues Tok { foo, bar, baz }```
  tsValues :: tok -> Record r -> TsBridgeM (Array DTS.TsDeclaration)

instance (TsValuesRL tok r rl, RowToList r rl) => TsValues tok r where
  tsValues tok r = tsValuesRL tok r (Proxy :: _ rl)

--------------------------------------------------------------------------------
-- class TsValuesRL
--------------------------------------------------------------------------------

class TsValuesRL :: Type -> Row Type -> RowList Type -> Constraint
class TsValuesRL tok r rl where
  tsValuesRL :: tok -> Record r -> Proxy rl -> TsBridgeM (Array DTS.TsDeclaration)

instance TsValuesRL tok r RL.Nil where
  tsValuesRL _ _ _ = pure []

instance
  ( TsValuesRL tok r rl
  , TsBridgeBy tok a
  , Row.Cons sym a rx r
  , IsSymbol sym
  ) =>
  TsValuesRL tok r (RL.Cons sym a rl) where
  tsValuesRL tok r _ = (<>) <$> head <*> tail
    where
    tail = tsValuesRL tok r (Proxy :: _ rl)
    head = tsValue' tok (reflectSymbol (Proxy :: _ sym)) (Proxy :: _ a)
