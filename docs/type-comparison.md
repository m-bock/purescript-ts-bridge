<h1>Type Comparison Reference</h1>

<!-- AUTO-GENERATED-CONTENT:START (TOC) -->
- [Number](#number)
- [String](#string)
- [Boolean](#boolean)
- [Array](#array)
- [Int](#int)
- [Maybe](#maybe)
- [Either](#either)
- [Tuple](#tuple)
- [Nullable](#nullable)
- [OneOf](#oneof)
- [Record](#record)
- [Variant](#variant)
- [VariantEncFlat](#variantencflat)
- [VariantEncNested](#variantencnested)
- [VariantEncNested](#variantencnested-1)
- [Function](#function)
- [Promise](#promise)
- [Effect](#effect)
- [Unit](#unit)
<!-- AUTO-GENERATED-CONTENT:END -->

The following is a list of default implementations for types that are provided in this library. Since the generation typeclass is defined on your side, you can choose a subset of the provided implementations.

In the future the following implementations may be added: 
- Uncurried Functions
- Native tuples
- Nonempty Arrays

<!-- AUTO-GENERATED-CONTENT:START (TYPES) -->

<table>
  
  <tr>
    <td colspan=3>
      <h2>Number</h2>

`Number` is represented as TypeScript builtin `number` type.

</td>
  </tr>
  <tr></tr>
  <tr>
    <th></th>
    <th align="left">PureScript</th>
    <th align="left">TypeScript</th>
  </tr>
  <tr></tr>


<tr>
  <td valign="top">Ref</td>
  <td valign="top">

```ts
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

`<builtin>`

</td>
<td valign="top">


`<builtin>`


  </td>
</tr>
<tr></tr>


  <tr>
    <td colspan=3>
      <h2>String</h2>

`String` is represented as TypeScript builtin `string` type.

</td>
  </tr>
  <tr></tr>
  <tr>
    <th></th>
    <th align="left">PureScript</th>
    <th align="left">TypeScript</th>
  </tr>
  <tr></tr>


<tr>
  <td valign="top">Ref</td>
  <td valign="top">

```ts
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

`<builtin>`

</td>
<td valign="top">


`<builtin>`


  </td>
</tr>
<tr></tr>


  <tr>
    <td colspan=3>
      <h2>Boolean</h2>

`Boolean` is represented as TypeScript builtin `boolean`.

</td>
  </tr>
  <tr></tr>
  <tr>
    <th></th>
    <th align="left">PureScript</th>
    <th align="left">TypeScript</th>
  </tr>
  <tr></tr>


<tr>
  <td valign="top">Ref</td>
  <td valign="top">

```ts
Boolean
```

</td>
<td valign="top">


```ts
boolean
```


  </td>
</tr>
<tr></tr>


<tr>
  <td valign="top">Def</td>
  <td valign="top">

`<builtin>`

</td>
<td valign="top">


`<builtin>`


  </td>
</tr>
<tr></tr>


  <tr>
    <td colspan=3>
      <h2>Array</h2>

`Array` is represented as TypeScript builtin `ReadonlyArray` type.

</td>
  </tr>
  <tr></tr>
  <tr>
    <th></th>
    <th align="left">PureScript</th>
    <th align="left">TypeScript</th>
  </tr>
  <tr></tr>


<tr>
  <td valign="top">Ref</td>
  <td valign="top">

```ts
Array String
```

</td>
<td valign="top">


```ts
ReadonlyArray<string>
```


  </td>
</tr>
<tr></tr>


<tr>
  <td valign="top">Ref</td>
  <td valign="top">

```ts
Unit -> Array a
```

</td>
<td valign="top">


```ts
<A>() => ReadonlyArray<A>
```


  </td>
</tr>
<tr></tr>


<tr>
  <td valign="top">Def</td>
  <td valign="top">

`<builtin>`

</td>
<td valign="top">


`<builtin>`


  </td>
</tr>
<tr></tr>


  <tr>
    <td colspan=3>
      <h2>Int</h2>

`Int` is represented as opaque type using TypeScript branded types. So there is no way to create an `Int` directly in TypeScript, you need to export a function like `round :: Number -> Int` and `toNumber :: Int -> Number` to construct and deconstruct an `Int`.

</td>
  </tr>
  <tr></tr>
  <tr>
    <th></th>
    <th align="left">PureScript</th>
    <th align="left">TypeScript</th>
  </tr>
  <tr></tr>


<tr>
  <td valign="top">Ref</td>
  <td valign="top">

```ts
Int
```

</td>
<td valign="top">


```ts
import('../Prim').Int
```


  </td>
</tr>
<tr></tr>


<tr>
  <td valign="top">Def</td>
  <td valign="top">

`<builtin>`

</td>
<td valign="top">


`output/Prim/index.d.ts`
```ts
type Int = {
  readonly __brand: unique symbol;
}
```


  </td>
</tr>
<tr></tr>


  <tr>
    <td colspan=3>
      <h2>Maybe</h2>

`Maybe` is represented as opaque type using TypeScript branded types. So there is no direct way to create a `Maybe` in TypeScript. See the FAQ for the general decision to represent ADTs as opaque types.

</td>
  </tr>
  <tr></tr>
  <tr>
    <th></th>
    <th align="left">PureScript</th>
    <th align="left">TypeScript</th>
  </tr>
  <tr></tr>


<tr>
  <td valign="top">Ref</td>
  <td valign="top">

```ts
Maybe a
```

</td>
<td valign="top">


```ts
import('../Data.Maybe').Maybe<A>
```


  </td>
</tr>
<tr></tr>


<tr>
  <td valign="top">Def</td>
  <td valign="top">

`~/Data/Maybe.purs`
```hs
data Maybe a
  = Just a
  | Nothing
```

</td>
<td valign="top">


`output/Data.Maybe/index.d.ts`
```ts
type Maybe<A> = {
  readonly __brand: unique symbol;
  readonly __arg1: A;
}
```


  </td>
</tr>
<tr></tr>


  <tr>
    <td colspan=3>
      <h2>Either</h2>

`Either` is represented as opaque type using TypeScript branded types. So there is no direct way to create a `Either` in TypeScript. See the FAQ for the general decision to represent ADTs as opaque types.

</td>
  </tr>
  <tr></tr>
  <tr>
    <th></th>
    <th align="left">PureScript</th>
    <th align="left">TypeScript</th>
  </tr>
  <tr></tr>


<tr>
  <td valign="top">Ref</td>
  <td valign="top">

```ts
Either a b
```

</td>
<td valign="top">


```ts
import('../Data.Either').Either<A, B>
```


  </td>
</tr>
<tr></tr>


<tr>
  <td valign="top">Def</td>
  <td valign="top">

`~/Data/Either.purs`
```hs
data Either a b
  = Left a
  | Right b
```

</td>
<td valign="top">


`output/Data.Either/index.d.ts`
```ts
type Either<A, B> = {
  readonly __brand: unique symbol;
  readonly __arg1: A;
  readonly __arg2: B;
}
```


  </td>
</tr>
<tr></tr>


  <tr>
    <td colspan=3>
      <h2>Tuple</h2>

`Tuple` is represented as opaque type using TypeScript branded types. So there is no direct way to create a `Either` in TypeScript. See the FAQ for the general decision to represent ADTs as opaque types.

</td>
  </tr>
  <tr></tr>
  <tr>
    <th></th>
    <th align="left">PureScript</th>
    <th align="left">TypeScript</th>
  </tr>
  <tr></tr>


<tr>
  <td valign="top">Ref</td>
  <td valign="top">

```ts
Tuple a b
```

</td>
<td valign="top">


```ts
import('../Data.Tuple').Tuple<A, B>
```


  </td>
</tr>
<tr></tr>


<tr>
  <td valign="top">Def</td>
  <td valign="top">

`~/Data/Tuple.purs`
```hs
data Tuple a b
  = Tuple a b
```

</td>
<td valign="top">


`output/Data.Tuple/index.d.ts`
```ts
type Tuple<A, B> = {
  readonly __brand: unique symbol;
  readonly __arg1: A;
  readonly __arg2: B;
}
```


  </td>
</tr>
<tr></tr>


  <tr>
    <td colspan=3>
      <h2>Nullable</h2>

From the [nullable](https://pursuit.purescript.org/packages/purescript-nullable) library.

`Nullable` is represented as TypeScript untagged union.

</td>
  </tr>
  <tr></tr>
  <tr>
    <th></th>
    <th align="left">PureScript</th>
    <th align="left">TypeScript</th>
  </tr>
  <tr></tr>


<tr>
  <td valign="top">Ref</td>
  <td valign="top">

```ts
Nullable a
```

</td>
<td valign="top">


```ts
import('../Data.Nullable').Nullable<A>
```


  </td>
</tr>
<tr></tr>


<tr>
  <td valign="top">Def</td>
  <td valign="top">

`~/Data/Nullable.purs`
```hs
foreign import data Nullable
  :: Type -> Type
```

</td>
<td valign="top">


`output/Data.Nullable/index.d.ts`
```ts
export type Nullable<A> = null | A
```


  </td>
</tr>
<tr></tr>


  <tr>
    <td colspan=3>
      <h2>OneOf</h2>

From the [untagged-union](https://pursuit.purescript.org/packages/purescript-untagged-union) library.

`OneOf` is represented as TypeScript untagged union.

</td>
  </tr>
  <tr></tr>
  <tr>
    <th></th>
    <th align="left">PureScript</th>
    <th align="left">TypeScript</th>
  </tr>
  <tr></tr>


<tr>
  <td valign="top">Ref</td>
  <td valign="top">

```ts
OneOf a b
```

</td>
<td valign="top">


```ts
import('../Untagged.Union').OneOf<A, B>
```


  </td>
</tr>
<tr></tr>


<tr>
  <td valign="top">Def</td>
  <td valign="top">

`~/Untagged/Union.purs`
```hs
foreign import data OneOf
  :: Type -> Type -> Type
```

</td>
<td valign="top">


`output/Untagged.Union/index.d.ts`
```ts
export type OneOf<A, B> = A | B
```


  </td>
</tr>
<tr></tr>


  <tr>
    <td colspan=3>
      <h2>Record</h2>

Records are represented as TypeScript records with readonly fields.

</td>
  </tr>
  <tr></tr>
  <tr>
    <th></th>
    <th align="left">PureScript</th>
    <th align="left">TypeScript</th>
  </tr>
  <tr></tr>


<tr>
  <td valign="top">Ref</td>
  <td valign="top">

```ts
{ name :: String
, loggedIn :: Boolean
}
```

</td>
<td valign="top">


```ts
{
  readonly name: string;
  readonly loggedIn: boolean;
}
```


  </td>
</tr>
<tr></tr>


<tr>
  <td valign="top">Def</td>
  <td valign="top">

`<builtin>`

</td>
<td valign="top">


`<builtin>`


  </td>
</tr>
<tr></tr>


  <tr>
    <td colspan=3>
      <h2>Variant</h2>

From the [variant](https://pursuit.purescript.org/packages/purescript-variant) library.

Variant types are represented as TypeScript tagged unions.

</td>
  </tr>
  <tr></tr>
  <tr>
    <th></th>
    <th align="left">PureScript</th>
    <th align="left">TypeScript</th>
  </tr>
  <tr></tr>


<tr>
  <td valign="top">Ref</td>
  <td valign="top">

```ts
Variant
  ( done :: String
  , counting :: Number
  , init :: Unit
  )
```

</td>
<td valign="top">


```ts
| {
    readonly type: 'done';
    readonly value: string;
  }
| {
    readonly type: 'counting';
    readonly value: number;
  }
| {
    readonly type: 'init';
    readonly value: void;
  }
```


  </td>
</tr>
<tr></tr>


<tr>
  <td valign="top">Def</td>
  <td valign="top">

`~/Data/Variant.purs`
```hs
foreign import data Variant
  :: Row Type -> Type
```

</td>
<td valign="top">


`<builtin>`


  </td>
</tr>
<tr></tr>


  <tr>
    <td colspan=3>
      <h2>VariantEncFlat</h2>

From the [variant-encodings](https://pursuit.purescript.org/packages/purescript-variant-encodings) library.

Flat encoded Variants are represented as TypeScript tagged unions.

</td>
  </tr>
  <tr></tr>
  <tr>
    <th></th>
    <th align="left">PureScript</th>
    <th align="left">TypeScript</th>
  </tr>
  <tr></tr>


<tr>
  <td valign="top">Ref</td>
  <td valign="top">

```ts
VariantEncFlat "kind"
  ( one :: (name :: String, size :: Number)
  , two :: (hobbies :: Array String)
  )
```

</td>
<td valign="top">


```ts
| {
    readonly kind: 'one';
    readonly name: string;
    readonly size: number;
  }
| {
    readonly kind: 'two';
    readonly hobbies: Array<string>;
  }
```


  </td>
</tr>
<tr></tr>


<tr>
  <td valign="top">Def</td>
  <td valign="top">

`~/Data/Variant/Encodings/Flat.purs`
```hs
foreign import data VariantEncFlat
  :: Symbol -> Row (Row Type) -> Type
```

</td>
<td valign="top">


`<builtin>`


  </td>
</tr>
<tr></tr>


  <tr>
    <td colspan=3>
      <h2>VariantEncNested</h2>

From the [variant-encodings](https://pursuit.purescript.org/packages/purescript-variant-encodings) library.

Variants with custom nested encoding are represented as TypeScript tagged unions.

</td>
  </tr>
  <tr></tr>
  <tr>
    <th></th>
    <th align="left">PureScript</th>
    <th align="left">TypeScript</th>
  </tr>
  <tr></tr>


<tr>
  <td valign="top">Ref</td>
  <td valign="top">

```ts
VariantEncNested "kind" "payload"
  ( one :: Number
  , two :: String
  )
```

</td>
<td valign="top">


```ts
| {
    readonly kind: 'one';
    readonly payload: number;
  }
| {
    readonly kind: 'two';
    readonly payload: string;
  }
```


  </td>
</tr>
<tr></tr>


<tr>
  <td valign="top">Def</td>
  <td valign="top">

`~/Data/Variant/Encodings/Nested.purs`
```hs
foreign import data VariantEncNested
  :: Symbol -> Symbol -> Row Type -> Type
```

</td>
<td valign="top">


`<builtin>`


  </td>
</tr>
<tr></tr>


  <tr>
    <td colspan=3>
      <h2>VariantEncNested</h2>

From the [variant-encodings](https://pursuit.purescript.org/packages/purescript-variant-encodings) library.

Variants with custom nested encoding are represented as TypeScript tagged unions.

</td>
  </tr>
  <tr></tr>
  <tr>
    <th></th>
    <th align="left">PureScript</th>
    <th align="left">TypeScript</th>
  </tr>
  <tr></tr>


<tr>
  <td valign="top">Ref</td>
  <td valign="top">

```ts
VariantEncNested "kind" "payload"
  ( one :: String
  , two :: Number
  )
```

</td>
<td valign="top">


```ts
| {
    readonly kind: 'one';
    readonly payload: string;
  }
| {
    readonly kind: 'two';
    readonly payload: number;
  }
```


  </td>
</tr>
<tr></tr>


<tr>
  <td valign="top">Def</td>
  <td valign="top">

`~/Data/Variant/Encodings/Nested.purs`
```hs
foreign import data VariantEncNested
  :: Symbol -> Symbol -> Row Type -> Type
```

</td>
<td valign="top">


`<builtin>`


  </td>
</tr>
<tr></tr>


  <tr>
    <td colspan=3>
      <h2>Function</h2>

Functions are represented as TypeScript curried functions.

</td>
  </tr>
  <tr></tr>
  <tr>
    <th></th>
    <th align="left">PureScript</th>
    <th align="left">TypeScript</th>
  </tr>
  <tr></tr>


<tr>
  <td valign="top">Ref</td>
  <td valign="top">

```ts
Number -> String -> Boolean
```

</td>
<td valign="top">


```ts
(_: number) => (_: string) => boolean
```


  </td>
</tr>
<tr></tr>


<tr>
  <td valign="top">Ref</td>
  <td valign="top">

```ts
forall a b c. a -> b -> c
```

</td>
<td valign="top">


```ts
<A>(_: A) =>
  <B, C>(_: B) =>
    C
```


  </td>
</tr>
<tr></tr>


<tr>
  <td valign="top">Def</td>
  <td valign="top">

`<builtin>`

</td>
<td valign="top">


`<builtin>`


  </td>
</tr>
<tr></tr>


  <tr>
    <td colspan=3>
      <h2>Promise</h2>

From the [aff-promise](https://pursuit.purescript.org/packages/purescript-aff-promise) library.

Promises are represented as TypeScript Promises. Note that in most cases it makes sense to treat them as `Effect (Promise a)`.

</td>
  </tr>
  <tr></tr>
  <tr>
    <th></th>
    <th align="left">PureScript</th>
    <th align="left">TypeScript</th>
  </tr>
  <tr></tr>


<tr>
  <td valign="top">Ref</td>
  <td valign="top">

```ts
Promise a
```

</td>
<td valign="top">


```ts
Promise<A>
```


  </td>
</tr>
<tr></tr>


<tr>
  <td valign="top">Def</td>
  <td valign="top">

`~/Control/Promise.purs`
```hs
foreign import data Promise :: Type -> Type
```

</td>
<td valign="top">


`<builtin>`


  </td>
</tr>
<tr></tr>


  <tr>
    <td colspan=3>
      <h2>Effect</h2>

Effects are represented as TypeScript functions.


</td>
  </tr>
  <tr></tr>
  <tr>
    <th></th>
    <th align="left">PureScript</th>
    <th align="left">TypeScript</th>
  </tr>
  <tr></tr>


<tr>
  <td valign="top">Ref</td>
  <td valign="top">

```ts
Effect a
```

</td>
<td valign="top">


```ts
<A>() => A
```


  </td>
</tr>
<tr></tr>


<tr>
  <td valign="top">Def</td>
  <td valign="top">

`~/Effect.purs`
```hs
foreign import data Effect
  :: Type -> Type
```

</td>
<td valign="top">


`<builtin>`


  </td>
</tr>
<tr></tr>


  <tr>
    <td colspan=3>
      <h2>Unit</h2>

`Unit` is represented as TypeScript's `void`


</td>
  </tr>
  <tr></tr>
  <tr>
    <th></th>
    <th align="left">PureScript</th>
    <th align="left">TypeScript</th>
  </tr>
  <tr></tr>


<tr>
  <td valign="top">Ref</td>
  <td valign="top">

```ts
Unit
```

</td>
<td valign="top">


```ts
void
```


  </td>
</tr>
<tr></tr>


<tr>
  <td valign="top">Def</td>
  <td valign="top">

`~/Data/Unit.purs`
```hs
foreign import data Unit
  :: Type
```

</td>
<td valign="top">


`<builtin>`


  </td>
</tr>


</table>

<!-- AUTO-GENERATED-CONTENT:END -->

