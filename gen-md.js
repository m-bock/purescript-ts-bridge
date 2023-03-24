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
      <h3>${title}</h3>

${text}
      </td>
  </tr>
  <tr></tr>
  <tr>
    <th></th>
    <th>PureScript</th>
    <th>TypeScript</th>
  </tr>
  <tr></tr>
`;

const Row_ = (
  { type_, codePurs, codeTs },
  children
) => `
<tr>
  <td valign="top">${type_}</td>
  <td valign="top">

\`\`\`hs
${codePurs}
\`\`\`

</td>
<td valign="top">

\`\`\`ts
${codeTs}
\`\`\`

  </td>
</tr>
<tr></tr>
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

      const tgt = fs.readFileSync("./output/Sample/index.d.ts").toString();
      return "```ts\n" + tgt + "```\n";
    },
    TYPES(content, options) {
      var data = [
        [
          "Number",
          "Number is represented as TypeScript builtin `number` type.",
          [
            ["Ref", "Number", "number"],
            [
              "Def",
              "<builtin>",
              "<builtin>",
            ],
          ],
        ],
        [
          "String",
          "String is represented as TypeScript builtin string type.",
          [
            ["Ref", "String", "string"],
            [
              "Def",
              "<builtin>",
              "<builtin>",
            ],
          ],
        ],
      ];

      return Table(
        {},
        data
          .map(
            ([title, text, samples]) =>
              [
                Row(
                  { title, text },
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

const markdownPath = path.join(
  __dirname,
  "README.md"
);
markdownMagic(markdownPath, config);
