module TsBridgeGen.Types where

import Prelude

newtype Glob = Glob String

newtype ModuleName = ModuleName String

newtype Name = Name String

data PursModule = PursModule ModuleName (Array PursDef)

data PursDef
  = DefData Name
  | DefNewtype Name
  | DefType Name
  | DefValue Name

