import * as prettier from "prettier";

export const runPrettierImpl = (source) => () => prettier.format(source, {parser: "json"});
