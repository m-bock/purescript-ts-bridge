-- A

module Sample where

-- ## Imports

import Prelude

import Effect (Effect)
import TsBridge as TSB
import TsBridge as TsBridge
import Type.Proxy (Proxy)

-- Define a type class

class TsBridge a where
  tsBridge :: Proxy a -> TSB.StandaloneTsType

-- Define a token

data Tok = Tok

instance TsBridge a => TSB.TsBridgeBy Tok a where
  tsBridgeBy _ = tsBridge

-- Define instances

instance TsBridge Number where
  tsBridge = TSB.defaultNumber

instance TsBridge String where
  tsBridge = TSB.defaultString

instance TsBridge a => TsBridge (Array a) where
  tsBridge = TSB.defaultArray Tok

instance (TsBridge a, TsBridge b) => TsBridge (a -> b) where
  tsBridge = TSB.defaultFunction Tok

instance (TSB.DefaultRecord Tok r) => TsBridge (Record r) where
  tsBridge = TSB.defaultRecord Tok

-- Add some PureScript values

foo :: Number
foo = 1.0

bar :: { x :: Number, y :: Number } -> String
bar _ = ""

-- Define a TypeScript program with one module

myTsProgram :: TSB.TsProgram
myTsProgram =
  TSB.tsProgram
    [ TSB.tsModuleFile "Sample"
        [ TSB.tsValues Tok
            { bar
            , foo
            }
        ]

    ]

-- Define an entry point for the code generator.

main :: Effect Unit
main = TsBridge.mkTypeGenCli myTsProgram