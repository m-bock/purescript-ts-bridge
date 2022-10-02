export {
  rename as renameImpl,
  truncate as truncateImpl,
  chown as chownImpl,
  chmod as chmodImpl,
  stat as statImpl,
  link as linkImpl,
  symlink as symlinkImpl,
  readlink as readlinkImpl,
  realpath as realpathImpl,
  unlink as unlinkImpl,
  rmdir as rmdirImpl,
  mkdir as mkdirImpl,
  readdir as readdirImpl,
  utimes as utimesImpl,
  readFile as readFileImpl,
  writeFile as writeFileImpl,
  appendFile as appendFileImpl,
  open as openImpl,
  read as readImpl,
  write as writeImpl,
  close as closeImpl
} from "fs";

export function handleCallbackImpl(left, right, f) {
  return function (err, value) {
    if (err) {
      f(left(err))();
    } else {
      f(right(value))();
    }
  };
}
