# purescript-typescript-bridge

- A PureScript library for type class based TypeScript type generation.

[sample-project](https://github.com/thought2/purescript-typescript-bridge.sample-project)

Features

- fully customizable. It's type class based, but you own the type class
- many default implementations to pick from
  - Number
  - Int
  - Char
  - String
  - Boolean
  - Function
  - Promise
  - Nullable
  - Array
  - Records
  - Variants
  - Effect
  - Unit

- Supports opaque types (implemented as branded types in TypeScript)
- Supports easily accessible Newtypes (implemented as branded types in TypeScript)
- Module resolution
- Polymorphic types