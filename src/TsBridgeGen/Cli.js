import minimatch from "minimatch";
import * as prettier from "prettier";

export const runPrettierImpl = (source) => () => prettier.format(source, {parser: "json"});

export const match = (pattern) => (str) => minimatch(str, pattern);
