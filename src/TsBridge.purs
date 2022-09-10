module TsBridge
  ( defaultProgOptions
  , module Exp
  , tsModuleFile
  , tsModuleWithImports
  , tsProgram
  , tsTypeAlias
  ) where

import Prelude

import Data.Typelevel.Undefined (undefined)
import TsBridge.Class (class TsBridge, toTsType)
import TsBridge.Class (class TsBridge, toTsType) as Exp
import TsBridge.DTS (TsDeclaration(..), TsFilePath(..), TsImport, TsModule(..), TsModuleFile(..), TsName(..), TsProgram(..))
import TsBridge.DTS (TsDeclaration(..), TsImport, TsModule(..), TsName(..), TsProgram(..), TsType, printTsModule, printTsProgram) as Exp
import Type.Proxy (Proxy)

tsModuleFile :: String -> Array (Array TsDeclaration) -> TsModuleFile
tsModuleFile n xs = TsModuleFile (TsFilePath n) (TsModule [] $ join xs)

tsModuleWithImports :: String -> Array TsImport -> Array (Array TsImport) -> TsModule
tsModuleWithImports = undefined

type ProgramOptions = { addImports :: Boolean }

defaultProgOptions :: ProgramOptions
defaultProgOptions = undefined

tsProgram :: ProgramOptions -> Array TsModuleFile -> TsProgram
tsProgram _ xs = TsProgram xs

-- tsModule :: String -> Array TsImport -> Array TsDeclaration -> TsModule
-- tsModule = undefined
tsTypeAlias :: forall a. TsBridge a => String -> Proxy a -> Array TsDeclaration
tsTypeAlias n p = [ TsDeclTypeDef (TsName n) [] $ toTsType p ]
