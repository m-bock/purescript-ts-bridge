export const empty = [];

export const cons = (x) => (xs) => [x, ...xs];

export const snoc = (xs) => (x) => [...xs, x];
