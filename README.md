# purescript-ts-bridge

<img
src="https://media.tenor.com/MRCIli40TYoAAAAi/under-construction90s-90s.gif" width="30">
<img
src="https://media.tenor.com/MRCIli40TYoAAAAi/under-construction90s-90s.gif" width="30">
<img
src="https://media.tenor.com/MRCIli40TYoAAAAi/under-construction90s-90s.gif" width="30">

A __PureScript__ library for type class based __TypeScript__ type generation (`.d.ts` files).

<!-- AUTO-GENERATED-CONTENT:START (TOC) -->
- [Features](#features)
- [Getting started](#getting-started)
- [Types](#types)
  - [Number](#number)
  - [String](#string)
- [Future features](#future-features)
- [FAQ](#faq)
- [Similar Projects](#similar-projects)
- [Support](#support)
<!-- AUTO-GENERATED-CONTENT:END -->

<h2>Features</h2>

 - Fully customizable. It's type class based, but the type class is defined on your side to ease selective instance implementations.
 - Many default implementations to pick from
 - Supports opaque types (implemented as branded types in TypeScript)
 - Supports easily accessible Newtypes
 - Module resolution
 - Polymorphic types


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
class TsBridge a where
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
  tsBridge = TSB.defaultNumber

instance TsBridge String where
  tsBridge = TSB.defaultString
```
Things get a bit more interesting for instances of generic types, like
the `Array` or `Function` type.
Here we need to pass the previously defined token so that the default
implementation can generate the generic type with the class that you defined.
```hs
instance TsBridge a => TsBridge (Array a) where
  tsBridge = TSB.defaultArray Tok

instance (TsBridge a, TsBridge b) => TsBridge (a -> b) where
  tsBridge = TSB.defaultFunction Tok
```
As you can see, this even works for something generic like records:
```hs
instance (TSB.DefaultRecord Tok r) => TsBridge (Record r) where
  tsBridge = TSB.defaultRecord Tok
```
</li>
<li>
We've defined a small set of types that we want to be able to generate
TypeScript equivalents from. Now we define some values of those types. 

```hs
foo :: Number
foo = 1.0

bar :: { x :: Number, y :: Number } -> String
bar _ = ""
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
            { bar
            , foo
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
export const bar: (_: { readonly x: number; readonly y: number }) => string;

export const foo: number;
```

<!-- AUTO-GENERATED-CONTENT:END -->

</li>
</ol>


<h2>Types</h2>

The following is a list of default implementations for types that are provided in this library. Since the generation typeclass is defined on your side, you can choose a subset of the provided implementations.


- Promise
- Variants
- Effect
- Unit


<!-- AUTO-GENERATED-CONTENT:START (TYPES) -->

<table>
  
  <tr>
    <td colspan=3>
      <h3>Number</h3>

Number is represented as TypeScript builtin `number` type.
      </td>
  </tr>
  <tr></tr>
  <tr>
    <th></th>
    <th>PureScript</th>
    <th>TypeScript</th>
  </tr>
  <tr></tr>


<tr>
  <td valign="top">Ref</td>
  <td valign="top">

```hs
Number
```

</td>
<td valign="top">

```ts
number
```

  </td>
</tr>
<tr></tr>


<tr>
  <td valign="top">Def</td>
  <td valign="top">

```hs
<builtin>
```

</td>
<td valign="top">

```ts
<builtin>
```

  </td>
</tr>
<tr></tr>


  <tr>
    <td colspan=3>
      <h3>String</h3>

String is represented as TypeScript builtin string type.
      </td>
  </tr>
  <tr></tr>
  <tr>
    <th></th>
    <th>PureScript</th>
    <th>TypeScript</th>
  </tr>
  <tr></tr>


<tr>
  <td valign="top">Ref</td>
  <td valign="top">

```hs
String
```

</td>
<td valign="top">

```ts
string
```

  </td>
</tr>
<tr></tr>


<tr>
  <td valign="top">Def</td>
  <td valign="top">

```hs
<builtin>
```

</td>
<td valign="top">

```ts
<builtin>
```

  </td>
</tr>
<tr></tr>

</table>

<!-- AUTO-GENERATED-CONTENT:END -->


<h2>Future features</h2>

- Uncurried Functions
- Native tuples

<h2>FAQ</h2>

- Q: Why are ADTs exported as opaque types. They're actually not opaque and it
  would be nice if they could be created and pattern matched on at the
  TypeScript side.

  A: The underlying representation of ADTs is not set in stone. Compilation
  backends differ in this point. For instance the official JS backend compiles
  them to OOP-ish structures in JavaScript. The newer optimized JS backend
  compiles them to discriminated unions as plain objects. Both could be
  represented in TypeScript, especially the latter is quite trivial.
  But this is not included in this library as it will make the code less
  portable.

- Q: If ADTs are fully opaque, how can I use them on the TypeScript side?

  A: If you export the constructors and a destructor function, you can use them to work with those types in TypeScript. For `Maybe` this would mean to export `just :: forall a. a -> Maybe a` and `nothing :: forall a. Unit -> Maybe a` and something like `onMaybe :: forall a z. (a -> z) -> (Unit -> z) -> Maybe a -> z`. Note that you have to redefine the ADT constructors as a plain function, you cannot export `Just :: forall a. a -> Maybe a` directly.
  It is easier to represent `Variant` types in TypeScript. Thus another option is to either use `Variant` in you interface or convert ADTs from and to `Variant` types. For the latter you can use a library like [labeled-data](https://github.com/thought2/purescript-labeled-data) for convenient conversions.

- Q: Is it safe to use PureScript code from TypeScript with the generated types?

  A: It depends. TypeScript still has the `any` type, which fits everywhere. You
  can avoid the `any` type in your codebase but they may sneak in through
  libraries. Also, TypeScript can perform arbitrary side effects at any place.
  If you export an interface that accepts a function of type `(_: number) => number` you can pass a function that does some IO.

- Q: TypeScript is a structurally typed language. PureScript has both, some structural qualities like the primitive types, records, arrays. And nominal part like ADTs and newtypes. the former is easy to express in TypeScript, but the latter how is it even possible?

  A: In TypeScript the technique of "branded types" is an approximation to nominal typing. If a type is defined like `type T = { readonly __brand: unique symbol; } & { a : number }` there is no way to directly construct a value of that type. The only way to construct a value of type `T` is with an explicit `as` conversion: `const x : T = { a: 12 } as T`.
  If you consider the `as` conversion as a `unsafeCoerce` this is good enough to represent opaque types. Unfortunately `as` conversions are also used for safe conversions or broadening in TypeScript, like `"a" as string | number`.

<h2>Similar Projects</h2>

- [purescript-tsd-gen](https://github.com/minoki/purescript-tsd-gen)
  This project follows a different approach for type generation. It extracts TypeScript types from the PureScript CST. As such the process is more automated but less customizable.

<h2>Support</h2>

<a href='https://ko-fi.com/C0C3HQFRF' target='_blank'><img height='36' style='border:0px;height:36px;' src='https://storage.ko-fi.com/cdn/kofi4.png?v=3' border='0' alt='Buy Me a Coffee at ko-fi.com' /></a>
