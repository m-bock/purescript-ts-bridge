import path from "path";
import markdownMagic from "markdown-magic";
import { fileURLToPath } from "url";
import * as fs from "fs";
import * as child_process from "child_process";
import { data } from "./data.js";
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
    ? "`<builtin>`"
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
    ? "`<builtin>`"
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
        "spago run --main Sample"
      );

      child_process.execSync(
        "yarn run prettier --write output/*/index.d.ts"
      );

      const tgt = fs
        .readFileSync(
          "./output/Sample/index.d.ts"
        )
        .toString();
      return "```ts\n" + tgt + "```\n";
    },
    TYPES(content, options) {
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
                    (
                      [
                        type_,
                        codePurs,
                        codeTs,
                      ],
                      ix2,
                      all2
                    ) =>
                      Row_(
                        {
                          type_,
                          codePurs,
                          codeTs,
                          isLast:
                            ix ===
                              all.length -
                                1 &&
                            ix2 ===
                              all2.length -
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
    path.join(
      __dirname,
      "../README.md"
    ),
    path.join(
      __dirname,
      "../docs/*.md"
    ),
  ],
  config
);
