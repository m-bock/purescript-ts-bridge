import path from "path";
import markdownMagic from "markdown-magic";
import { fileURLToPath } from "url";
import * as fs from "fs";
import * as child_process from "child_process";

import * as pursToMd from "purs-to-md";

const __filename = fileURLToPath(
  import.meta.url
);

const __dirname =
  path.dirname(__filename);

const Table = (_, children) => `
<table>
  ${children}
</table>
`;

const Row = (
  { title, text },
  children
) => `
  <tr>
    <td colspan=3>
      <h2>${title}</h2>

${text}
      </td>
  </tr>
  <tr></tr>
  <tr>
    <th></th>
    <th align="left">PureScript</th>
    <th align="left">TypeScript</th>
  </tr>
  <tr></tr>
`;

const Row_ = (
  { type_, codePurs, codeTs, isLast },
  children
) => `
<tr>
  <td valign="top">${type_}</td>
  <td valign="top">

${
  codePurs == null
    ? "`builtin`"
    : typeof codePurs === "string"
    ? ["```ts", codePurs, "```"].join(
        "\n"
      )
    : [
        "`" + codePurs[0] + "`",
        "```hs",
        codePurs[1],
        "```",
      ].join("\n")
}

</td>
<td valign="top">


${
  codeTs === null
    ? "`builtin`"
    : typeof codeTs === "string"
    ? ["```ts", codeTs, "```"].join(
        "\n"
      )
    : [
        "`" + codeTs[0] + "`",
        "```ts",
        codeTs[1],
        "```",
      ].join("\n")
}


  </td>
</tr>
${isLast ? "" : "<tr></tr>"}
`;

const config = {
  matchWord: "AUTO-GENERATED-CONTENT",
  transforms: {
    SAMPLE() {
      const src = fs
        .readFileSync(
          "test/Sample.purs"
        )
        .toString();
      return pursToMd.convert(src);
    },
    SAMPLE_OUTPUT() {
      child_process.execSync(
        "spago run --main Sample -a '--prettier node_modules/.bin/prettier'"
      );

      const tgt = fs
        .readFileSync(
          "./output/Sample/index.d.ts"
        )
        .toString();
      return "```ts\n" + tgt + "```\n";
    },
    TYPES(content, options) {
      var data = [
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
            [
              "Ref",
              "Boolean",
              "boolean",
            ],
            ["Def", null, null],
          ],
        ],
        [
          "Array",
          "`Array` is represented as TypeScript builtin `ReadonlyArray` type.",
          [
            [
              "Ref",
              "Array a",
              "ReadonlyArray<A>",
            ],
            ["Def", null, null],
          ],
        ],
        [
          "Int",
          "`Int` is represented as opaque type using TypeScript branded types. So there is no way to create an `Int` directly in TypeScript, you need to export a functions like `round :: Number -> Int` and `toNumber :: Int -> Number` to construct and deconstruct an `Int`.",
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
                  "  readonly __arg0: A;",
                  "}",
                ].join("\n"),
              ],
            ],
          ],
        ],
        [
          "Either",
          "`Either` is represented as opaque type using TypeScript __branded types. So there is no direct way to create a `Either` in TypeScript. See the FAQ for the general decision to represent ADTs as opaque types.",
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
                  "  readonly __arg0: A;",
                  "  readonly __arg1: B;",
                  "}",
                ].join("\n"),
              ],
            ],
          ],
        ],
        // [
        //   "",
        //   "",
        //   [
        //     ["Ref", "", ""],
        //     ["Def", "", ""],
        //   ],
        // ],
        // [
        //   "",
        //   "",
        //   [
        //     ["Ref", "", ""],
        //     ["Def", "", ""],
        //   ],
        // ],
        // [
        //   "",
        //   "",
        //   [
        //     ["Ref", "", ""],
        //     ["Def", "", ""],
        //   ],
        // ],
        // [
        //   "",
        //   "",
        //   [
        //     ["Ref", "", ""],
        //     ["Def", "", ""],
        //   ],
        // ],
        // [
        //   "",
        //   "",
        //   [
        //     ["Ref", "", ""],
        //     ["Def", "", ""],
        //   ],
        // ],
      ];

      return Table(
        {},
        data
          .map(
            (
              [title, text, samples],
              ix,
              all
            ) =>
              [
                Row(
                  {
                    title,
                    text,
                  },
                  ""
                ),
                samples
                  .map(
                    ([
                      type_,
                      codePurs,
                      codeTs,
                    ]) =>
                      Row_(
                        {
                          type_,
                          codePurs,
                          codeTs,
                          isLast:
                            ix ===
                            all.length -
                              1,
                        },
                        ""
                      )
                  )
                  .join("\n"),
              ].join("\n")
          )
          .join("\n")
      );
    },
  },
  callback: function () {
    console.log(
      "markdown processing done"
    );
  },
};

export default config;

markdownMagic(
  [
    path.join(__dirname, "README.md"),
    path.join(__dirname, "docs/*.md"),
  ],
  config
);
