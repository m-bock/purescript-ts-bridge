-- Create a new module inside the spago project.

module Sample where

-- The folowing imports are needed for this example:

import Prelude
import Effect (Effect)
import TsBridge as TSB
import Type.Proxy (Proxy)

-- Then you should define a typeclass that looks like this: 

class TsBridge a where
  tsBridge :: Proxy a -> TSB.StandaloneTsType

-- Now we need to tell `ts-bridge` that it should use your typeclass for the type
-- generation. We do this by defining a simple data type `Tok` which we use in the following instance
-- for the library's internal type class `TsBridgeBy`.

data Tok = Tok

instance TsBridge a => TSB.TsBridgeBy Tok a where
  tsBridgeBy _ = tsBridge

-- Now we can define instances for types. As you can see below `ts-bridge`
-- provides some useful default implementations thay you can use:

instance TsBridge Number where
  tsBridge = TSB.defaultNumber

instance TsBridge String where
  tsBridge = TSB.defaultString

-- Things get a bit more interesting for instances of generic types, like
-- the `Array` or `Function` type.
-- Here we need to pass the previously defined token so that the default
-- implementation can generate the generic type with the class that you defined.

instance TsBridge a => TsBridge (Array a) where
  tsBridge = TSB.defaultArray Tok

instance (TsBridge a, TsBridge b) => TsBridge (a -> b) where
  tsBridge = TSB.defaultFunction Tok

-- As you can see, this even works for something generic like records:

instance (TSB.DefaultRecord Tok r) => TsBridge (Record r) where
  tsBridge = TSB.defaultRecord Tok

-- We've defined a small set of types that we want to be able to generate
-- TypeScript equivalents from. Now we define some values of those types. 

foo :: Number
foo = 1.0

bar :: { x :: Number, y :: Number } -> String
bar _ = ""

-- Then we define a program that has one module. Note that the name of the
-- module must match the real name of the PureScript module.
-- The same for the values that we want to expose. However, we're making use of
-- record puns to eliminate the risk of speling mistakes:

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

-- Finaly we define an entry point for the code generator:

main :: Effect Unit
main = TSB.mkTypeGenCli myTsProgram