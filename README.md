# purescript-ts-bridge

A PureScript library for type class based TypeScript type generation (.d.ts Files).

<!-- AUTO-GENERATED-CONTENT:START (TOC) -->
- [Getting started](#getting-started)
- [Features](#features)
- [Types](#types)
  - [Number](#number)
  - [String](#string)
  - [Boolean](#boolean)
  - [Array](#array)
  - [Int](#int)
  - [Maybe](#maybe)
  - [Nullable](#nullable)
  - [Records](#records)
- [Support](#support)
<!-- AUTO-GENERATED-CONTENT:END -->

<h2>Getting started</h2>

```
spago install ts-bridge
```

The best way to get started is to have a look at the
[demo-project](https://github.com/thought2/purescript-ts-bridge.demo-project).


<h2>Features</h2>

- Fully customizable. It's type class based, but the type class is defined on your side to ease instance implementations.
- Many default implementations to pick from
- Supports opaque types (implemented as branded types in TypeScript)
- Supports easily accessible Newtypes (implemented as branded types in TypeScript)
- Module resolution
- Polymorphic types

<h2>Types</h2>

The following is a list of default implementations for types that are provided in this library. Since the generation typeclass is defined on your side, you can choose a subset of the provided implementations.

<table>

  <tr>
    <td colspan=3>
      <h3>Number</h3>
      <code>Number</code> is represented as TypeScript builtin <code>number</code> type
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
      <pre>Number</pre>
    </td>
    <td valign="top">
      <pre>number</pre>
    </td>
  </tr>
  <tr></tr>
  <tr>
    <td valign="top">Def</td>
    <td valign="top">
&lt;builtin&gt;
    </td>
    <td valign="top">
&lt;builtin&gt;
</td>
  </tr>
  <tr></tr>


  <tr>
    <td colspan=3>
      <h3>String</h3>
      <code>String</code> is represented as TypeScript builtin <code>string</code> type.
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
      <pre>String</pre>
    </td>
    <td valign="top">
      <pre>string</pre>
    </td>
  </tr>
  <tr></tr>
  <tr>
    <td valign="top">Def</td>
    <td valign="top">
&lt;builtin&gt;
    </td>
    <td valign="top">
&lt;builtin&gt;
</td>
  </tr>
  <tr></tr>


  <tr>
    <td colspan=3>
      <h3>Boolean</h3>
      <code>Boolean</code> is represented as TypeScript builtin <code>boolean</code> type.
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
      <pre>Boolean</pre>
    </td>
    <td valign="top">
      <pre>boolean</pre>
    </td>
  </tr>
  <tr></tr>
  <tr>
    <td valign="top">Def</td>
    <td valign="top">
&lt;builtin&gt;
    </td>
    <td valign="top">
&lt;builtin&gt;
</td>
  </tr>
  <tr></tr>


  <tr>
    <td colspan=3>
      <h3>Array</h3>
      <code>Array</code> is represented as TypeScript builtin <code>ReadonlyArray</code> type.
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
      <pre>Array a</pre>
    </td>
    <td valign="top">
      <pre>ReadonlyArray&lt;A&gt;</pre>
    </td>
  </tr>
  <tr></tr>
  <tr>
    <td valign="top">Def</td>
    <td valign="top">
&lt;builtin&gt;
    </td>
    <td valign="top">
&lt;builtin&gt;
</td>
  </tr>
  <tr></tr>


  <tr>
    <td colspan=3>
      <h3>Int</h3>
      <code>Int</code> is represented as opaque type using TypeScript branded types. So there is no way to create an `Int` directly in TypeScript, you need to export a functions like <code>round :: Number -> Int</code> and <code>toNumber :: Int -> Number</code> to construct and deconstruct an `Int`.
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
      <pre>Int</pre>
    </td>
    <td valign="top">
      <pre>import('../Prim').Int</pre>
    </td>
  </tr>
  <tr></tr>
  <tr>
    <td valign="top">Def</td>
    <td valign="top">
&lt;builtin&gt;
    </td>
    <td valign="top">
output/Prim/index.d.ts
<pre>
type Int = {
  readonly __brand: unique symbol;
};
</pre></td>
  </tr>
  <tr></tr>


  <tr>
    <td colspan=3>
      <h3>Char</h3>
      <code>Char</code> is represented as opaque type using TypeScript branded types. So there is no way to create a `Char` directly in TypeScript, you need to export a constructor and destructor functions, similar to <code>Int</code>. 
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
      <pre>Char</pre>
    </td>
    <td valign="top">
      <pre>import('../Prim').Int</pre>
    </td>
  </tr>
  <tr></tr>
  <tr>
    <td valign="top">Def</td>
    <td valign="top">
&lt;builtin&gt;
    </td>
    <td valign="top">
output/Prim/index.d.ts
<pre>
type Char = {
  readonly __brand: unique symbol;
};
</pre></td>
  </tr>
  <tr></tr>


  <tr>
    <td colspan=3>
      <h3>Maybe</h3>
      <code>Maybe</code> is represented as opaque type using TypeScript branded types. so there is no direct way to create a <code>Maybe</code> in TypeScript. See the FAQ for the general decision to represent ADTs as opaque types.  
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
      <pre>Maybe a</pre>
    </td>
    <td valign="top">
      <pre>import('../Data.Maybe').Maybe&lt;A&gt;</pre>
    </td>
  </tr>
  <tr></tr>
  <tr>
    <td valign="top">Def</td>
    <td valign="top">
~/Data/Maybe.purs

```hs
module Data.Maybe where

data Maybe a
  = Just a
  | Nothing
```

</td>
    <td valign="top">
output/Data.Maybe/index.d.ts

```ts
export type Maybe<A> = {
  readonly __brand: unique symbol;
  readonly __arg0: A;
};
```

</td>
  </tr>
  <tr></tr>


  <tr>
    <td colspan=3>
      <h3>Tuple</h3>
      <code>Tuple</code> is represented as opaque type using TypeScript __branded types. so there is no direct way to create a <code>Tuple</code> in TypeScript. See the FAQ for the general decision to represent ADTs as opaque types.  
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
      <pre>Tuple a</pre>
    </td>
    <td valign="top">
      <pre>import('../Data.Tuple').Tuple&lt;A&gt;</pre>
    </td>
  </tr>
  <tr></tr>
  <tr>
    <td valign="top">Def</td>
    <td valign="top">
~/Data/Tuple.purs
<pre>
module Data.Tuple where
&nbsp;
data Tuple a b = Tuple a b
</pre>
    </td>
    <td valign="top">
output/Data.Tuple/index.d.ts
<pre>
export type Tuple&lt;A, B&gt; = {
  readonly __brand: unique symbol;
  readonly __arg0: A;
  readonly __arg1: B;
};
</pre></td>
  </tr>
  <tr></tr>



  <tr>
    <td colspan=3>
      <h3>Either</h3>
      <code>Either</code> is represented as opaque type using TypeScript __branded types. so there is no direct way to create a <code>Either</code> in TypeScript. See the FAQ for the general decision to represent ADTs as opaque types.  
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
      <pre>Either a</pre>
    </td>
    <td valign="top">
      <pre>import('../Data.Either').Either&lt;A&gt;</pre>
    </td>
  </tr>
  <tr></tr>
  <tr>
    <td valign="top">Def</td>
    <td valign="top">
~/Data/Either.purs
<pre>
module Data.Either where
&nbsp;
data Either a b = Left a | Right b
</pre>
    </td>
    <td valign="top">
output/Data.Either/index.d.ts
<pre>
export type Either&lt;A, B&gt; = {
  readonly __brand: unique symbol;
  readonly __arg0: A;
  readonly __arg1: B;
};
</pre></td>
  </tr>
  <tr></tr>


  <tr>
    <td colspan=3>
      <h3>Nullable</h3>
      <code>Nullable</code> is represented as TypeScript untagged union.
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
      <pre>Nullable a</pre>
    </td>
    <td valign="top">
      <pre>import('../Data.Nullable').Nullable&lt;A&gt;</pre>
    </td>
  </tr>
  <tr></tr>
  <tr>
    <td valign="top">Def</td>
    <td valign="top">
~/Data/Nullable.purs
<pre>
module Data.Nullable where
&nbsp;
foreign import data Nullable 
  :: Type -> Type
</pre>
    </td>
    <td valign="top">
output/Data.Nullable/index.d.ts
<pre>
export type Nullable&lt;A&gt; = null | A;
</pre></td>
  </tr>
  <tr></tr>

  <tr>
    <td colspan=3>
      <h3>Records</h3>
      Records are represented as TypeScript records with readonly fields.
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
<pre>
{ name :: String
, loggedIn :: Boolean
}
</pre>
    </td>
    <td valign="top">
<pre>
{
  readonly name: string;
  readonly loggedIn: boolean;
}
</pre>
    </td>
  </tr>
  <tr></tr>
  <tr>
    <td valign="top">Def</td>
    <td valign="top">&lt;builtin&gt;</td>
    <td valign="top">&lt;builtin&gt;</td>
  </tr>

</table>

  - Function
  - Promise
  - Variants
  - Effect
  - Unit

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
  If you export an interface that accepts a function of type `(_: number) =>
  number` you can pass a function that does some IO.

- Q: TypeScript is a structurally typed language. PureScript has both, some structural qualities like the primitive types, records, arrays. And nominal part like ADTs and newtypes. the former is easy to express in TypeScript, but the latter how is it even possible?
  
  A: In TypeScript the technique of "branded types" is an approximation to nominal typing. If a type is defined like `type T = { readonly __brand: unique symbol; } & { a : number }` there is no way to directly construct a value of that type. The only way to construct a value of type `T` is with an explicit `as` conversion: `const x : T = { a: 12 } as T`.
  If you consider the `as` conversion as a `unsafeCoerce` this is good enough to represent opaque types. Unfortunately `as` conversions are also used for safe conversions or broadening in TypeScript, like `"a" as string | number`.

<h2>Similar Projects</h2>

- [purescript-tsd-gen](https://github.com/minoki/purescript-tsd-gen)
  This project follows a different approach for type generation. It extracts TypeScript types from the PureScript CST. As such the process is more automated but less customizable.

<h2>Support</h2>

<a href='https://ko-fi.com/C0C3HQFRF' target='_blank'><img height='36' style='border:0px;height:36px;' src='https://storage.ko-fi.com/cdn/kofi4.png?v=3' border='0' alt='Buy Me a Coffee at ko-fi.com' /></a>