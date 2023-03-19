module TsBridge.Core
  ( class TsBridgeBy
  , class TsValues
  , class TsValuesRL
  , tsValuesRL
  , tsBridgeBy
  , tsModuleFile
  , tsProgram
  , tsValue
  , tsValues
  ) where

import Prelude

import Control.Monad.Writer (listens, tell)
import Data.Array (uncons)
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Set as Set
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\))
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Safe.Coerce (coerce)
import TsBridge.DTS (TsBridge_DTS_Wrap(..), TsDeclVisibility(..), TsDeclaration(..), TsModule(..), TsModuleFile(..), TsName(..), TsProgram(..), TsType, dtsFilePath)
import TsBridge.Monad (TsBridgeAccum(..), TsBridgeM, TsBridge_Monad_Wrap(..), runTsBridgeM)
import Type.Proxy (Proxy(..))

class TsBridgeBy tok a where
  tsBridgeBy :: tok -> a -> TsBridgeM TsType

tsModuleFile :: String -> Array (TsBridgeM (Array TsDeclaration)) -> Array TsModuleFile
tsModuleFile n xs =
  let
    (xs' /\ TsBridgeAccum { typeDefs }) = runTsBridgeM $ join <$> sequence xs
  in
    typeDefs <> [ TsModuleFile (dtsFilePath n) (TsModule Set.empty xs') ]

mergeModules :: Array TsModuleFile -> TsProgram
mergeModules xs =
  xs
    <#> (\(TsModuleFile mp m) -> mp /\ m)
    # Map.fromFoldableWith mergeModule
    # TsProgram

mergeModule :: TsModule -> TsModule -> TsModule
mergeModule (TsModule is1 ds1) (TsModule is2 ds2) =
  TsModule
    (is1 `Set.union` is2)
    (Array.nub (ds1 <> ds2))

tsProgram :: Array (Array TsModuleFile) -> TsProgram
tsProgram xs = mergeModules $ join xs

tsTypeAlias :: forall mp a. TsBridgeBy mp a => mp -> String -> a -> TsBridgeM (Array TsDeclaration)
tsTypeAlias mp n x = ado
  x /\ scope <- listens (un TsBridgeAccum >>> _.scope) t
  in [ TsDeclTypeDef (TsName n) Public (coerce scope.floating) x ]
  where
  t = tsBridgeBy mp x

tsOpaqueType :: forall mp a. TsBridgeBy mp a => mp -> String -> a -> TsBridgeM (Array TsDeclaration)
tsOpaqueType mp n x = do
  _ /\ modules <- listens (un TsBridgeAccum >>> _.typeDefs) $ tsBridgeBy mp x
  case uncons modules of
    Just { head: (TsModuleFile _ (TsModule imports decls)), tail: [] } -> do
      tell $ TsBridgeAccum
        { typeDefs: mempty
        , scope: mempty
        }
      pure decls
    _ -> pure []

tsValue :: forall mp a. TsBridgeBy mp a => mp -> String -> a -> TsBridgeM (Array TsDeclaration)
tsValue mp n x = do
  t <- tsBridgeBy mp x
  pure [ TsDeclValueDef (TsName n) Public t ]

--------------------------------------------------------------------------------
-- class TsValues
--------------------------------------------------------------------------------

class TsValues tok r where
  tsValues :: tok -> Record r -> TsBridgeM (Array TsDeclaration)

instance (TsValuesRL tok r rl, RowToList r rl) => TsValues tok r where
  tsValues tok r = tsValuesRL tok r (Proxy :: _ rl)

--------------------------------------------------------------------------------
-- class TsValuesRL
--------------------------------------------------------------------------------

class TsValuesRL :: Type -> Row Type -> RowList Type -> Constraint
class TsValuesRL tok r rl where
  tsValuesRL :: tok -> Record r -> Proxy rl -> TsBridgeM (Array TsDeclaration)

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
    head = tsValue tok (reflectSymbol (Proxy :: _ sym)) (Record.get (Proxy :: _ sym) r)
