module TsBridge
  ( module Exp
  , tsModuleFile
  , tsModuleWithImports
  , tsProgram
  , tsTypeAlias
  ) where

import Prelude

import Control.Monad.Writer (runWriter)
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Undefined (undefined)
import TsBridge.Class (class ToTsBridge, TsBridge, runTsBridge, toTsBridge)
import TsBridge.Class (class ToTsBridge, toTsBridge) as Exp
import TsBridge.DTS (TsDeclaration(..), TsFilePath(..), TsImport, TsModule(..), TsModuleFile(..), TsName(..), TsProgram(..))
import TsBridge.DTS (TsDeclaration(..), TsImport, TsModule(..), TsName(..), TsProgram(..), TsType, printTsProgram) as Exp
import Type.Proxy (Proxy)

tsModuleFile :: String -> Array (TsBridge (Array TsDeclaration)) -> TsModuleFile
tsModuleFile n xs =
  let
    (xs' /\ w) = runTsBridge $ join <$> sequence xs
  in
    TsModuleFile (TsFilePath n) (TsModule [] xs')

tsModuleWithImports :: String -> Array TsImport -> Array (Array TsImport) -> TsModule
tsModuleWithImports = undefined

tsProgram :: Array TsModuleFile -> TsProgram
tsProgram xs = TsProgram xs

tsTypeAlias :: forall a. ToTsBridge a => String -> Proxy a -> TsBridge (Array TsDeclaration)
tsTypeAlias n p = ado
  x <- toTsBridge p
  in [ TsDeclTypeDef (TsName n) [] x ]
