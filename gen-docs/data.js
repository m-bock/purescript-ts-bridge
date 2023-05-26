export const data = [
  [
    "Number",
    "`Number` is represented as TypeScript builtin `number` type.",
    [
      ["Ref", "Number", "number"],
      ["Def", null, null],
    ],
  ],
  [
    "String",
    "`String` is represented as TypeScript builtin `string` type.",
    [
      ["Ref", "String", "string"],
      ["Def", null, null],
    ],
  ],
  [
    "Boolean",
    "`Boolean` is represented as TypeScript builtin `boolean`.",
    [
      ["Ref", "Boolean", "boolean"],
      ["Def", null, null],
    ],
  ],
  [
    "Array",
    "`Array` is represented as TypeScript builtin `ReadonlyArray` type.",
    [
      [
        "Ref",
        "Array String",
        "ReadonlyArray<string>",
      ],
      [
        "Ref",
        "Unit -> Array a",
        "<A>() => ReadonlyArray<A>",
      ],
      ["Def", null, null],
    ],
  ],
  [
    "Int",
    "`Int` is represented as opaque type using TypeScript branded types. So there is no way to create an `Int` directly in TypeScript, you need to export a function like `round :: Number -> Int` and `toNumber :: Int -> Number` to construct and deconstruct an `Int`.",
    [
      [
        "Ref",
        "Int",
        "import('../Prim').Int",
      ],
      [
        "Def",
        null,
        [
          "output/Prim/index.d.ts",
          [
            "type Int = {",
            "  readonly __brand: unique symbol;",
            "}",
          ].join("\n"),
        ],
      ],
    ],
  ],
  [
    "Maybe",
    "`Maybe` is represented as opaque type using TypeScript branded types. So there is no direct way to create a `Maybe` in TypeScript. See the FAQ for the general decision to represent ADTs as opaque types.",
    [
      [
        "Ref",
        "Maybe a",
        "import('../Data.Maybe').Maybe<A>",
      ],
      [
        "Def",
        [
          "~/Data/Maybe.purs",
          [
            "data Maybe a",
            "  = Just a",
            "  | Nothing",
          ].join("\n"),
        ],
        [
          "output/Data.Maybe/index.d.ts",
          [
            "type Maybe<A> = {",
            "  readonly __brand: unique symbol;",
            "  readonly __arg1: A;",
            "}",
          ].join("\n"),
        ],
      ],
    ],
  ],
  [
    "Either",
    "`Either` is represented as opaque type using TypeScript branded types. So there is no direct way to create a `Either` in TypeScript. See the FAQ for the general decision to represent ADTs as opaque types.",
    [
      [
        "Ref",
        "Either a b",
        "import('../Data.Either').Either<A, B>",
      ],
      [
        "Def",
        [
          "~/Data/Either.purs",
          [
            "data Either a b",
            "  = Left a",
            "  | Right b",
          ].join("\n"),
        ],
        [
          "output/Data.Either/index.d.ts",
          [
            "type Either<A, B> = {",
            "  readonly __brand: unique symbol;",
            "  readonly __arg1: A;",
            "  readonly __arg2: B;",
            "}",
          ].join("\n"),
        ],
      ],
    ],
  ],
  [
    "Tuple",
    "`Tuple` is represented as opaque type using TypeScript branded types. So there is no direct way to create a `Either` in TypeScript. See the FAQ for the general decision to represent ADTs as opaque types.",
    [
      [
        "Ref",
        "Tuple a b",
        "import('../Data.Tuple').Tuple<A, B>",
      ],
      [
        "Def",
        [
          "~/Data/Tuple.purs",
          [
            "data Tuple a b",
            "  = Tuple a b",
          ].join("\n"),
        ],
        [
          "output/Data.Tuple/index.d.ts",
          [
            "type Tuple<A, B> = {",
            "  readonly __brand: unique symbol;",
            "  readonly __arg1: A;",
            "  readonly __arg2: B;",
            "}",
          ].join("\n"),
        ],
      ],
    ],
  ],
  [
    "Nullable",
    [
      "From the [nullable](https://pursuit.purescript.org/packages/purescript-nullable) library.",
      "",
      "`Nullable` is represented as TypeScript untagged union.",
    ].join("\n"),
    [
      [
        "Ref",
        "Nullable a",
        "import('../Data.Nullable').Nullable<A>",
      ],
      [
        "Def",
        [
          "~/Data/Nullable.purs",
          [
            "foreign import data Nullable",
            "  :: Type -> Type",
          ].join("\n"),
        ],
        [
          "output/Data.Nullable/index.d.ts",
          [
            "export type Nullable<A> = null | A",
          ].join("\n"),
        ],
        ,
      ],
    ],
  ],
  [
    "OneOf",
    [
      "From the [untagged-union](https://pursuit.purescript.org/packages/purescript-untagged-union) library.",
      "",
      "`OneOf` is represented as TypeScript untagged union.",
    ].join("\n"),
    [
      [
        "Ref",
        "OneOf a b",
        "import('../Untagged.Union').OneOf<A, B>",
      ],
      [
        "Def",
        [
          "~/Untagged/Union.purs",
          [
            "foreign import data OneOf",
            "  :: Type -> Type -> Type",
          ].join("\n"),
        ],
        [
          "output/Untagged.Union/index.d.ts",
          [
            "export type OneOf<A, B> = A | B",
          ].join("\n"),
        ],
        ,
      ],
    ],
  ],
  [
    "Record",
    "Records are represented as TypeScript records with readonly fields.",
    [
      [
        "Ref",
        [
          "{ name :: String",
          ", loggedIn :: Boolean",
          "}",
        ].join("\n"),
        [
          "{",
          "  readonly name: string;",
          "  readonly loggedIn: boolean;",
          "}",
        ].join("\n"),
      ],
      ["Def", null, null],
    ],
  ],
  [
    "Variant",
    [
      "From the [variant](https://pursuit.purescript.org/packages/purescript-variant) library.",
      "",
      "Variant types are represented as TypeScript tagged unions.",
    ].join("\n"),
    [
      [
        "Ref",
        [
          "Variant",
          "  ( done :: String",
          "  , counting :: Number",
          "  , init :: Unit",
          "  )",
        ].join("\n"),
        [
          "| {",
          "    readonly type: 'done';",
          "    readonly value: string;",
          "  }",
          "| {",
          "    readonly type: 'counting';",
          "    readonly value: number;",
          "  }",
          "| {",
          "    readonly type: 'init';",
          "    readonly value: void;",
          "  }",
        ].join("\n"),
      ],
      [
        "Def",
        [
          "~/Data/Variant.purs",
          [
            "foreign import data Variant",
            "  :: Row Type -> Type",
          ].join("\n"),
        ],
        null,
      ],
    ],
  ],
  [
    "VariantEncFlat",
    [
      "From the [variant-encodings](https://pursuit.purescript.org/packages/purescript-variant-encodings) library.",
      "",
      "Flat encoded Variants are represented as TypeScript tagged unions.",
    ].join("\n"),
    [
      [
        "Ref",
        [
          "VariantEncFlat \"kind\"",
          "  ( one :: {name :: String, size :: Number}",
          "  , two :: {hobbies :: Array String}",
          "  )",
        ].join("\n"),
        [
          "| {",
          "    readonly kind: 'one';",
          "    readonly name: string;",
          "    readonly size: number;",
          "  }",
          "| {",
          "    readonly kind: 'two';",
          "    readonly hobbies: Array<string>;",
          "  }",
        ].join("\n"),
      ],
      [
        "Def",
        [
          "~/Data/Variant/Encodings/Flat.purs",
          [
            "foreign import data VariantEncFlat",
            "  :: Symbol -> Row (Row Type) -> Type",
          ].join("\n"),
        ],
        null,
      ],
    ],
  ],
  [
    "VariantEncNested",
    [
      "From the [variant-encodings](https://pursuit.purescript.org/packages/purescript-variant-encodings) library.",
      "",
      "Variants with custom nested encoding are represented as TypeScript tagged unions.",
    ].join("\n"),
    [
      [
        "Ref",
        [
          "VariantEncNested \"kind\" \"payload\"",
          "  ( one :: Number",
          "  , two :: String",
          "  )",
        ].join("\n"),
        [
          "| {",
          "    readonly kind: 'one';",
          "    readonly payload: number;",
          "  }",
          "| {",
          "    readonly kind: 'two';",
          "    readonly payload: string;",
          "  }",
        ].join("\n"),
      ],
      [
        "Def",
        [
          "~/Data/Variant/Encodings/Nested.purs",
          [
            "foreign import data VariantEncNested",
            "  :: Symbol -> Symbol -> Row Type -> Type",
          ].join("\n"),
        ],
        null,
      ],
    ],
  ],
  [
    "VariantEncNested",
    [
      "From the [variant-encodings](https://pursuit.purescript.org/packages/purescript-variant-encodings) library.",
      "",
      "Variants with custom nested encoding are represented as TypeScript tagged unions.",
    ].join("\n"),
    [
      [
        "Ref",
        [
          "VariantEncNested \"kind\" \"payload\"",
          "  ( one :: String",
          "  , two :: Number",
          "  )",
        ].join("\n"),
        [
          "| {",
          "    readonly kind: 'one';",
          "    readonly payload: string;",
          "  }",
          "| {",
          "    readonly kind: 'two';",
          "    readonly payload: number;",
          "  }",
        ].join("\n"),
      ],
      [
        "Def",
        [
          "~/Data/Variant/Encodings/Nested.purs",
          [
            "foreign import data VariantEncNested",
            "  :: Symbol -> Symbol -> Row Type -> Type",
          ].join("\n"),
        ],
        null,
      ],
    ],
  ],
  [
    "Function",
    "Functions are represented as TypeScript curried functions.",
    [
      [
        "Ref",
        [
          "Number -> String -> Boolean",
        ].join("\n"),
        [
          "(_: number) => (_: string) => boolean",
        ].join("\n"),
      ],
      [
        "Ref",
        [
          "forall a b c. a -> b -> c",
        ].join("\n"),
        [
          "<A>(_: A) =>",
          "  <B, C>(_: B) =>",
          "    C",
        ].join("\n"),
      ],
      ["Def", null, null],
    ],
  ],
  [
    "Promise",
    [
      "From the [aff-promise](https://pursuit.purescript.org/packages/purescript-aff-promise) library.",
      "",
      "Promises are represented as TypeScript Promises. Note that in most cases it makes sense to treat them as `Effect (Promise a)`.",
    ].join("\n"),
    [
      [
        "Ref",
        ["Promise a"].join("\n"),
        ["Promise<A>"].join("\n"),
      ],
      [
        "Def",
        [
          "~/Control/Promise.purs",
          [
            "foreign import data Promise :: Type -> Type",
          ].join("\n"),
        ],
        null,
      ],
    ],
  ],
  [
    "Effect",
    [
      "Effects are represented as TypeScript functions.",
      "",
    ].join("\n"),
    [
      [
        "Ref",
        ["Effect a"].join("\n"),
        ["<A>() => A"].join("\n"),
      ],
      [
        "Def",
        [
          "~/Effect.purs",
          [
            "foreign import data Effect",
            "  :: Type -> Type"
          ].join("\n"),
        ],
        null,
      ],
    ],
  ],
  [
    "Unit",
    [
      "`Unit` is represented as TypeScript's `void`",
      "",
    ].join("\n"),
    [
      [
        "Ref",
        ["Unit"].join("\n"),
        ["void"].join("\n"),
      ],
      [
        "Def",
        [
          "~/Data/Unit.purs",
          [
            "foreign import data Unit",
            "  :: Type"
          ].join("\n"),
        ],
        null,
      ],
    ],
  ],
];
