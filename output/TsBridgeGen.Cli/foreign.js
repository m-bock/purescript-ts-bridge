import minimatch from "minimatch";

export const match = (pattern) => (str) => minimatch(str, pattern);
