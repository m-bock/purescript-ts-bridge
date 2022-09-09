module TsBridge where

import Prelude

import Data.Typelevel.Undefined (undefined)
import Effect (Effect)
import Type.Proxy (Proxy)

data TsModule

data TsProgram

data TsDeclaration

data TsImport

data TsType

type ProgramOptions = { addImports :: Boolean }

class TsBridge a where
  toTsType :: a -> TsType

tsProgram :: Array TsModule -> TsProgram
tsProgram = undefined

tsModule :: String -> Array TsImport -> Array TsDeclaration -> TsModule
tsModule = undefined

tsTypeAlias :: forall a. TsBridge a => String -> Proxy a -> Array TsDeclaration
tsTypeAlias = undefined

mkTypeGenCli :: TsProgram -> Effect Unit
mkTypeGenCli = undefined