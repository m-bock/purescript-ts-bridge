module TsBridge
  ( module Exp
  , tsModuleFile
  , tsModuleWithImports
  , tsProgram
  , tsTypeAlias
  ) where

import Prelude

import Control.Monad.Writer (listens)
import Data.Array as Array
import Data.Map as Map
import Data.Newtype (un)
import Data.Set as Set
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Undefined (undefined)
import Safe.Coerce (coerce)
import TsBridge.Class (class ToTsBridge, toTsBridge)
import TsBridge.Class (class ToTsBridge, toTsBridge) as Exp
import TsBridge.DTS (TsDeclaration(..), TsImport, TsModule(..), TsModuleFile(..), TsName(..), TsProgram(..), dtsFilePath)
import TsBridge.DTS (TsDeclaration(..), TsImport, TsModule(..), TsName(..), TsProgram(..), TsType) as Exp
import TsBridge.Monad (TsBridgeAccum(..), TsBridgeM, runTsBridgeM)
import TsBridge.Print (printTsProgram) as Exp
import Type.Proxy (Proxy)
import TsBridge.DTS as TsBridge.DTS

tsModuleFile :: String -> Array (TsBridgeM (Array TsDeclaration)) -> Array TsModuleFile
tsModuleFile n xs =
  let
    (xs' /\ TsBridgeAccum { typeDefs, imports }) = runTsBridgeM $ join <$> sequence xs
  in
    typeDefs <> [ TsModuleFile (dtsFilePath n) (TsModule imports xs') ]

mergeModules :: Array TsModuleFile -> TsProgram
mergeModules xs = xs
  <#> (\(TsModuleFile mp m) -> mp /\ m)
  # Map.fromFoldableWith mergeModule
  # TsProgram

mergeModule :: TsModule -> TsModule -> TsModule
mergeModule (TsModule is1 ds1) (TsModule is2 ds2) = TsModule
  (is1 `Set.union` is2)
  (Array.nub (ds1 <> ds2))

tsModuleWithImports :: String -> Array TsImport -> Array (Array TsImport) -> TsModule
tsModuleWithImports = undefined

tsProgram :: Array (Array TsModuleFile) -> TsProgram
tsProgram xs = mergeModules $ join xs

tsTypeAlias :: forall a. ToTsBridge a => String -> Proxy a -> TsBridgeM (Array TsDeclaration)
tsTypeAlias n p = ado
  x /\ scope <- listens (un TsBridgeAccum >>> _.scope) $ toTsBridge p
  in [ TsDeclTypeDef (TsName n) (coerce scope.floating) x ]

