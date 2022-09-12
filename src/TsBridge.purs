module TsBridge
  ( module Exp
  , tsModuleFile
  , tsModuleWithImports
  , tsProgram
  , tsTypeAlias
  ) where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Set as Set
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Undefined (undefined)
import TsBridge.Class (class ToTsBridge, TsBridge, runTsBridge, toTsBridge)
import TsBridge.Class (class ToTsBridge, toTsBridge) as Exp
import TsBridge.DTS (TsDeclaration(..), TsFilePath(..), TsImport, TsModule(..), TsModuleFile(..), TsName(..), TsProgram(..))
import TsBridge.DTS (TsDeclaration(..), TsImport, TsModule(..), TsName(..), TsProgram(..), TsType, printTsProgram) as Exp
import Type.Proxy (Proxy)

tsModuleFile :: String -> Array (TsBridge (Array TsDeclaration)) -> Array TsModuleFile
tsModuleFile n xs =
  let
    (xs' /\ { typeDefs, imports }) = runTsBridge $ join <$> sequence xs
  in
    typeDefs <> [ TsModuleFile (TsFilePath n) (TsModule imports xs') ]

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

tsTypeAlias :: forall a. ToTsBridge a => String -> Proxy a -> TsBridge (Array TsDeclaration)
tsTypeAlias n p = ado
  x <- toTsBridge p
  in [ TsDeclTypeDef (TsName n) [] x ]
