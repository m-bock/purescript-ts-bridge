import * as jd from 'jest-diff';

export const diff = (x) => (y) => jd.diffStringsUnified(x,y)

export const stringify = (s) => JSON.stringify(s, null, 2)