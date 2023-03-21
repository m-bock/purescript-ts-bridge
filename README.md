# purescript-typescript-bridge

- A PureScript library for type class based TypeScript type generation (.d.ts Files).

The best way to get started is to have a look at the
[sample-project](https://github.com/thought2/purescript-typescript-bridge.sample-project).

## Features

- Fully customizable. It's type class based, but the type class is defined on your side to ease instance implementations.
- Many default implementations to pick from

  - Number
  - String
  - Boolean
  - Array

  - Int
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

### Number

|     | PureScript  |     | TypeScript  |
| --- | ----------- | --- | ----------- |
| Ref | `Number`    | <>  | `number`    |
| Def | `<builtin>` | <>  | `<builtin>` |

### String

|     | PureScript  |     | TypeScript  |
| --- | ----------- | --- | ----------- |
| Ref | `String`    | <>  | `string`    |
| Def | `<builtin>` | <>  | `<builtin>` |

### Boolean

|     | PureScript  |     | TypeScript  |
| --- | ----------- | --- | ----------- |
| Ref | `Boolean`   | <>  | `boolean`   |
| Def | `<builtin>` | <>  | `<builtin>` |

### Array

|     | PureScript  |     | TypeScript  |
| --- | ----------- | --- | ----------- |
| Ref | `Array a`   | <>  | `Array<A>`  |
| Def | `<builtin>` | <>  | `<builtin>` |

### Int

<table>
  <tr>
    <th></th>
    <th>PureScript</th>
    <th>TypeScript</th>
  </tr>
  <tr>
    <td valign="top">Ref</td>
    <td valign="top">
      <pre>Int</pre>
    </td>
    <td valign="top">
      <pre>import('../Prim.Int').Int</pre>
    </td>
  </tr>
  <tr>
    <td valign="top">Def</td>
    <td valign="top">
<pre>
&lt;builtin&gt;
</pre>
    </td>
    <td valign="top">
output/Prim.Int/index.d.ts
<pre>
type Int = {
  readonly brand: unique symbol;
};
</pre></td>
  </tr>
</table>


### Maybe

<table>
  <tr>
    <td colspan=3>
      <h2>Int</h2>
      <code>Int</code> is represented as opque type using TypeScript branded types.
    </td>
  </tr>
  <tr><td colspan=3></td></tr>
  <tr>
    <th></th>
    <th>PureScript</th>
    <th>TypeScript</th>
  </tr>
  <tr><td colspan=3></td></tr>
  <tr>
    <td valign="top">Ref</td>
    <td valign="top">
      <pre>Int</pre>
    </td>
    <td valign="top">
      <code>import('../Prim.Int').Int</code>
    </td>
  </tr>
  <tr><td colspan=3></td></tr>
  <tr>
    <td valign="top">Def</td>
    <td valign="top">
<pre>
&lt;builtin&gt;
</pre>
    </td>
    <td valign="top">
output/Prim.Int/index.d.ts
<code><pre>
type Int = {
  readonly brand: unique symbol;
};
</pre></code></td>
  </tr>
  <tr><td colspan=3></td></tr>
  <tr>
    <td colspan=3>
      <h2>Maybe</h2>
      Maybe is represented as opaque type using TypeScript branded types.
    </td>
  </tr>
  <tr><td colspan=3></td></tr>
  <tr>
    <th></th>
    <th>PureScript</th>
    <th>TypeScript</th>
  </tr>
  <tr><td colspan=3></td></tr>
  <tr>
    <td valign="top">Ref</td>
    <td valign="top">
      <pre>Maybe a</pre>
    </td>
    <td valign="top">
      <pre>import('../Data.Maybe').Maybe&lt;A&gt;</pre>
    </td>
  </tr>
  <tr><td colspan=3></td></tr>
  <tr>
    <td valign="top">Def</td>
    <td valign="top">
<pre>
module Data.Maybe where

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
  <tr><td colspan=3></td></tr>
</table>

- Supports opaque types (implemented as branded types in TypeScript)
- Supports easily accessible Newtypes (implemented as branded types in TypeScript)
- Module resolution
- Polymorphic types
