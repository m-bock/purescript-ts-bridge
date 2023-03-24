# purescript-ts-bridge

<img
src="https://media.tenor.com/MRCIli40TYoAAAAi/under-construction90s-90s.gif" width="30">
<img
src="https://media.tenor.com/MRCIli40TYoAAAAi/under-construction90s-90s.gif" width="30">
<img
src="https://media.tenor.com/MRCIli40TYoAAAAi/under-construction90s-90s.gif" width="30">

A **PureScript** library for type class based **TypeScript** type generation (`.d.ts` files).

## Documentation

- [Getting Started Guide](https://pursuit.purescript.org/packages/purescript-ts-bridge)
- [API Docs on Pursuit](https://pursuit.purescript.org/packages/purescript-ts-bridge)
- [FAQs](docs/faq.md)
- [Type Comparison](docs/type-comparison.md)
- [Demo repo](https://github.com/thought2/purescript-ts-bridge.demo)

## Installation

```
spago install ts-bridge
```

## Features

- Fully customizable. It's type class based, but the type class is defined on your side to ease selective instance implementations.
- Many default implementations to pick from
- Supports opaque types (implemented as branded types in TypeScript)
- Supports easily accessible Newtypes
- Module resolution
- Polymorphic types

## Future features

- Uncurried Functions
- Native tuples
- Nonempty Arrays

## Similar Projects

- [purescript-tsd-gen](https://github.com/minoki/purescript-tsd-gen)
  This project follows a different approach for type generation. It extracts TypeScript types from the PureScript CST. As such the process is more automated but less customizable.

# Support

If you find a bug or have a feature idea feel free to make a PR or file an issue.

<a href='https://ko-fi.com/C0C3HQFRF' target='_blank'><img height='36' style='border:0px;height:36px;' src='https://storage.ko-fi.com/cdn/kofi4.png?v=3' border='0' alt='Buy Me a Coffee at ko-fi.com' /></a>
