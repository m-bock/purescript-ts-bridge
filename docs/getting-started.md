<h2>Getting started</h2>

The best way to get started is to have a look at the
[demo-project](https://github.com/thought2/purescript-ts-bridge.demo).


<ol>
<!-- AUTO-GENERATED-CONTENT:START (SAMPLE) -->
<li>
Create a new module inside the spago project.

```hs
module Sample where
```
</li>
<li>
The folowing imports are needed for this example:

```hs
import Prelude
import Effect (Effect)
import TsBridge as TSB
import Type.Proxy (Proxy)
```
</li>
<li>
Then you should define a typeclass that looks like this: 

```hs
class TsBridge (a :: Type) where
  tsBridge :: Proxy a -> TSB.StandaloneTsType
```
</li>
<li>
Now we need to tell `ts-bridge` that it should use your typeclass for the type
generation. We do this by defining a simple data type `Tok` which we use in the following instance
for the library's internal type class `TsBridgeBy`.

```hs
data Tok = Tok

instance TsBridge a => TSB.TsBridgeBy Tok a where
  tsBridgeBy _ = tsBridge
```
</li>
<li>
Now we can define instances for types. As you can see below `ts-bridge`
provides some useful default implementations thay you can use:

```hs
instance TsBridge Number where
  tsBridge = TSB.tsBridgeNumber

instance TsBridge String where
  tsBridge = TSB.tsBridgeString
```
Things get a bit more interesting for instances of generic types, like
the `Array` or `Function` type.
Here we need to pass the previously defined token so that the tsBridge
implementation can generate the generic type with the class that you defined.
```hs
instance TsBridge a => TsBridge (Array a) where
  tsBridge = TSB.tsBridgeArray Tok

instance (TsBridge a, TsBridge b) => TsBridge (a -> b) where
  tsBridge = TSB.tsBridgeFunction Tok
```
As you can see, this even works for something generic like records:
```hs
instance (TSB.TsBridgeRecord Tok r) => TsBridge (Record r) where
  tsBridge = TSB.tsBridgeRecord Tok
```
</li>
<li>
We've defined a small set of types that we want to be able to generate
TypeScript equivalents from. Now we define some values of those types. 

```hs
gravity :: Number
gravity = 9.81

points :: Array { x :: Number, y :: Number }
points = [ { x: 0.0, y: 0.0 }, { x: 3.0, y: 2.0 } ]

greet :: String -> String
greet name = "Hello, " <> name <> "!"

type Person =
  { name :: String
  , hobbies :: Array String
  , coordinates :: { x :: Number, y :: Number }
  }

person :: Person
person =
  { name: "Santa"
  , hobbies: [ "running", "swimming" ]
  , coordinates: { x: 13.2, y: 0.7 }
  }
```
</li>
<li>
Then we define a program that has one module. Note that the name of the
module must match the real name of the PureScript module.
The same for the values that we want to expose. However, we're making use of
record puns to eliminate the risk of speling mistakes:

```hs
myTsProgram :: TSB.TsProgram
myTsProgram =
  TSB.tsProgram
    [ TSB.tsModuleFile "Sample"
        [ TSB.tsValues Tok
            { gravity
            , points
            , greet
            , person
            }
        ]

    ]
```
</li>
<li>
Finaly we define an entry point for the code generator:

```hs
main :: Effect Unit
main = TSB.mkTypeGenCli myTsProgram
```
</li>
<!-- AUTO-GENERATED-CONTENT:END -->

<li>
And we can run this CLI with `spago`:

```
spago run --main App -a '--prettier "node_modules/.bin/prettier"'
```

Thereafter the file `output/Sample/index.d.ts` should contain the generated types for this module.

<!-- AUTO-GENERATED-CONTENT:START (SAMPLE_OUTPUT) -->
```ts
export const gravity: number;

export const greet: (_: string) => string;

export const person: {
  readonly coordinates: { readonly x: number; readonly y: number };
  readonly hobbies: Array<string>;
  readonly name: string;
};

export const points: Array<{ readonly x: number; readonly y: number }>;
```

<!-- AUTO-GENERATED-CONTENT:END -->

</li>
</ol>
