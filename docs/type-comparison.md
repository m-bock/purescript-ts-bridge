<h1>Type Comparison Reference</h1>

<!-- AUTO-GENERATED-CONTENT:START (TOC) -->
- [Number](#number)
- [String](#string)
- [Boolean](#boolean)
- [Array](#array)
- [Int](#int)
- [Maybe](#maybe)
- [Either](#either)
<!-- AUTO-GENERATED-CONTENT:END -->

The following is a list of default implementations for types that are provided in this library. Since the generation typeclass is defined on your side, you can choose a subset of the provided implementations.

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
    <th>PureScript</th>
    <th>TypeScript</th>
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

`builtin`

</td>
<td valign="top">


`builtin`


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
    <th>PureScript</th>
    <th>TypeScript</th>
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

`builtin`

</td>
<td valign="top">


`builtin`


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
    <th>PureScript</th>
    <th>TypeScript</th>
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

`builtin`

</td>
<td valign="top">


`builtin`


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
    <th>PureScript</th>
    <th>TypeScript</th>
  </tr>
  <tr></tr>


<tr>
  <td valign="top">Ref</td>
  <td valign="top">

```ts
Array a
```

</td>
<td valign="top">


```ts
ReadonlyArray<A>
```


  </td>
</tr>
<tr></tr>


<tr>
  <td valign="top">Def</td>
  <td valign="top">

`builtin`

</td>
<td valign="top">


`builtin`


  </td>
</tr>
<tr></tr>


  <tr>
    <td colspan=3>
      <h2>Int</h2>

`Int` is represented as opaque type using TypeScript branded types. So there is no way to create an `Int` directly in TypeScript, you need to export a functions like `round :: Number -> Int` and `toNumber :: Int -> Number` to construct and deconstruct an `Int`.
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

`builtin`

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
    <th>PureScript</th>
    <th>TypeScript</th>
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
  readonly __arg0: A;
}
```


  </td>
</tr>
<tr></tr>


  <tr>
    <td colspan=3>
      <h2>Either</h2>

`Either` is represented as opaque type using TypeScript __branded types. So there is no direct way to create a `Either` in TypeScript. See the FAQ for the general decision to represent ADTs as opaque types.
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
  readonly __arg0: A;
  readonly __arg1: B;
}
```


  </td>
</tr>


</table>

<!-- AUTO-GENERATED-CONTENT:END -->
