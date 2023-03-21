# purescript-typescript-bridge


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
spago install typescript-bridge
```

The best way to get started is to have a look at the
[sample-project](https://github.com/thought2/purescript-typescript-bridge.sample-project).


<h2>Features</h2>

- Fully customizable. It's type class based, but the type class is defined on your side to ease instance implementations.
- Many default implementations to pick from
- Supports opaque types (implemented as branded types in TypeScript)
- Supports easily accessible Newtypes (implemented as branded types in TypeScript)
- Module resolution
- Polymorphic types

<h2>Types</h2>

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
      <code>String</code> is represented as TypeScript builtin <code>string</code> type
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
      <code>Boolean</code> is represented as TypeScript builtin <code>boolean</code> type
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
      <code>Array</code> is represented as TypeScript builtin <code>ReadonlyArray</code> type
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
      <code>Int</code> is represented as opaque type using TypeScript branded types.
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
      <pre>import('../Prim.Int').Int</pre>
    </td>
  </tr>
  <tr></tr>
  <tr>
    <td valign="top">Def</td>
    <td valign="top">
&lt;builtin&gt;
    </td>
    <td valign="top">
output/Prim.Int/index.d.ts
<pre>
type Int = {
  readonly brand: unique symbol;
};
</pre></td>
  </tr>
  <tr></tr>

  <tr>
    <td colspan=3>
      <h3>Maybe</h3>
      <code>Maybe</code> is represented as opaque type using TypeScript branded types.
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
<pre>
module Data.Maybe where
&nbsp;
data Maybe a = Just a | Nothing
</pre>
    </td>
    <td valign="top">
output/Data.Maybe/index.d.ts
<pre>
export type Maybe&lt;A&gt; = {
  readonly brand: unique symbol;
  readonly arg0: A;
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


  - Char
  - Function
  - Promise
  - Nullable
  - Records
  - Variants
  - Effect
  - Unit
  - Maybe
  - Either
  - Tuple

<h2>Support</h2>

<a href='https://ko-fi.com/C0C3HQFRF' target='_blank'><img height='36' style='border:0px;height:36px;' src='https://storage.ko-fi.com/cdn/kofi4.png?v=3' border='0' alt='Buy Me a Coffee at ko-fi.com' /></a>