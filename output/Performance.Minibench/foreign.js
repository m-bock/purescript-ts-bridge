export const hrTime = process.hrtime;

export function gc() {
  global.gc && global.gc();
}

export function toFixed(n) {
  return n.toFixed(2);
}
