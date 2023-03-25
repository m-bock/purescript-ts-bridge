# purescript-ts-bridge

<img
src="https://media.tenor.com/MRCIli40TYoAAAAi/under-construction90s-90s.gif" width="30">

A **PureScript** library for type class based **TypeScript** type generation (`.d.ts` files).

## Documentation

- [Getting Started Guide](docs/getting-started.md)
- [API Docs on Pursuit](https://pursuit.purescript.org/packages/purescript-ts-bridge)
- [FAQs](docs/faq.md)
- [Type Comparison PureScript vs. TypeScript](docs/type-comparison.md)
- [Demo repo](https://github.com/thought2/purescript-ts-bridge.demo)

## Installation

```
spago install ts-bridge
```

## Features

- Fully customizable via a user defined type class pattern
- Many default implementations to pick from (Primitives, Records, Variants, ...)
- Opaque types (implemented as branded types in TypeScript)
- Easily accessible Newtypes
- Module resolution
- Polymorphic types optimized for best type inference in TS
- Tried and tested in production


## Similar Projects

- [purescript-tsd-gen](https://github.com/minoki/purescript-tsd-gen)
  This project follows a different approach for type generation. It extracts TypeScript types from the PureScript CST. As such the process is more automated but less customizable.

# Support

If you find a bug or have a feature idea feel free to make a PR or file an issue. Or just...

<a href='https://ko-fi.com/C0C3HQFRF' target='_blank'><img height='36' style='border:0px;height:36px;' src='https://storage.ko-fi.com/cdn/kofi4.png?v=3' border='0' alt='Buy Me a Coffee at ko-fi.com' /></a>
