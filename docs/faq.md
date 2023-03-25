
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

  A: In TypeScript the technique of "branded types" is an approximation to nominal typing. If a type is defined like `type T = { readonly __brand: unique symbol; }` there is no way to directly construct a value of that type. The only way to construct a value of type `T` is with an explicit `as` conversion: `const x : T = 17 as unknown as T`.
  If you consider the `as unknown as` conversion as a `unsafeCoerce` this is good enough to represent opaque types.
