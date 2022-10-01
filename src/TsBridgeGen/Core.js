import * as prettier from "prettier";

export const runPrettier = (source) => () => prettier.format(source);
