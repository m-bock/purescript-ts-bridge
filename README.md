# purescript-typescript-bridge

- A PureScript library for type class based TypeScript type generation (.d.ts Files).

The best way to get started is to have a look at the 
[sample-project](https://github.com/thought2/purescript-typescript-bridge.sample-project).

## Features

- Fully customizable. It's type class based, but the type class is defined on your side to ease instance implementations.
- Many default implementations to pick from
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
  - Maybe
  - Either
  - Tuple

- Supports opaque types (implemented as branded types in TypeScript)
- Supports easily accessible Newtypes (implemented as branded types in TypeScript)
- Module resolution
- Polymorphic types