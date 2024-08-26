// @ts-check

import fs from "node:fs";
import path from "node:path";
import process from "node:process";
import { BitArray, Ok, Error as GError, toList } from "./gleam.mjs";
import * as $simplifile from "./simplifile.mjs";

/**
 * Read the contents of a file as a BitArray
 *
 * @param {string} filepath
 * @returns {Ok | GError} A Result with the BitArray of the file contents
 */
export function readBits(filepath) {
  return gleamResult(() => {
    const contents = fs.readFileSync(path.normalize(filepath));
    return new BitArray(new Uint8Array(contents));
  });
}

/**
 * Write the given BitArray to a file
 *
 * @param {string} filepath
 * @param {BitArray} contents
 * @returns {Ok | GError}
 */
export function writeBits(filepath, contents) {
  return gleamResult(() => fs.writeFileSync(path.normalize(filepath), contents.buffer));
}

/**
 * Append the given BitArray to a file
 *
 * @param {string} filepath
 * @param {BitArray} contents
 * @returns {Ok | GError}
 */
export function appendBits(filepath, contents) {
  return gleamResult(() => fs.appendFileSync(path.normalize(filepath), contents.buffer));
}

/**
 * Check whether a file exists at the given path
 *
 * @param {string} filepath
 * @returns {Ok | GError}
 */
export function isFile(filepath) {
  try {
    return new Ok(fs.statSync(path.normalize(filepath)).isFile());
  } catch (e) {
    if (e.code === "ENOENT") {
      return new Ok(false);
    } else {
      return new GError(cast_error(e.code));
    }
  }
}

/**
 * Check whether a symbolic link exists at the given path
 *
 * @param {string} filepath
 * @returns {Ok | GError}
 */
export function isSymlink(filepath) {
  try {
    return new Ok(fs.lstatSync(path.normalize(filepath)).isSymbolicLink());
  } catch (e) {
    if (e.code === "ENOENT") {
      return new Ok(false);
    } else {
      return new GError(cast_error(e.code));
    }
  }
}

/**
 * Check whether a directory exists at the given path
 *
 * @param {string} filepath
 * @returns {Ok | GError}
 */
export function isDirectory(filepath) {
  try {
    return new Ok(fs.statSync(path.normalize(filepath)).isDirectory());
  } catch (e) {
    if (e.code === "ENOENT") {
      return new Ok(false);
    } else {
      return new GError(cast_error(e.code));
    }
  }
}

/**
 * Create the symbolic link called path pointing to target
 *
 * @param {string} target
 * @param {string} path
 * @returns {Ok | GError}
 */
export function createSymlink(target, path) {
  return gleamResult(() => fs.symlinkSync(target, path));
}

/**
 * Create a directory at the given filepath
 *
 * @param {string} filepath
 * @returns {Ok | GError}
 */
export function createDirectory(filepath) {
  return gleamResult(() => fs.mkdirSync(path.normalize(filepath)));
}

/**
 * Recursively create a directory structure from the given path.
 *
 * @param {string} filepath
 * @returns {Ok | GError}
 */
export function createDirAll(filepath) {
  return gleamResult(() => {
    fs.mkdirSync(path.normalize(filepath), { recursive: true });
  });
}

/**
 * Recursively delete a directory or delete a file at the given path.
 *
 * @param {string} fileOrDirPath
 * @returns {Ok | GError}
 */
export function delete_(fileOrDirPath) {
  return gleamResult(() => {
    const isDir = isDirectory(fileOrDirPath);
    if (isDir instanceof Ok && isDir[0] === true) {
      fs.rmSync(path.normalize(fileOrDirPath), { recursive: true });
    } else {
      fs.unlinkSync(path.normalize(fileOrDirPath));
    }
  });
}

/**
 * List the contents of a directory.
 *
 * @param {string} filepath
 * @returns {Ok | GError}
 */
export function readDirectory(filepath) {
  return gleamResult(() => toList(fs.readdirSync(path.normalize(filepath))));
}

/**
 * Copy a file to a new path.
 *
 * @param {string} srcpath
 * @param {string} destpath
 * @returns {Ok | GError}
 */
export function copyFile(srcpath, destpath) {
  return gleamResult(() => fs.copyFileSync(path.normalize(srcpath), path.normalize(destpath)));
}

/**
 * Move a file to the new path.
 *
 * @param {string} srcpath
 * @param {string} destpath
 * @returns {Ok | GError}
 */
export function renameFile(srcpath, destpath) {
  return gleamResult(() => fs.renameSync(path.normalize(srcpath), path.normalize(destpath)));
}

/**
 * Set the file permissions. Octal number should be in base 8.
 *
 * @param {string} filepath
 * @param {number} octalNumber
 * @returns {Ok | GError}
 */
export function setPermissionsOctal(filepath, octalNumber) {
  return gleamResult(() => fs.chmodSync(path.normalize(filepath), octalNumber));
}

/**
 * Return the current directory.
 *
 * @returns {Ok | GError} The current directory
 */
export function currentDirectory() {
  return gleamResult(() => process.cwd());
}

/**
 *
 * @param {string} filepath
 * @returns {Ok | GError}
 */
export function fileInfo(filepath) {
  return gleamResult(() => {
    const stat = fs.statSync(path.normalize(filepath))
    return new FileInfo(stat)
  });
}

/**
 * @param {string} filepath
 * @returns {Ok | GError}
 */
export function linkInfo(filepath) {
  return gleamResult(() => {
    const stat = fs.lstatSync(path.normalize(filepath))
    return new FileInfo(stat)
  })
}

class FileInfo {
  /**
   * @param {fs.Stats} stat
   */
  constructor(stat) {
    this.size = stat.size;
    this.mode = stat.mode;
    this.nlinks = stat.nlink;
    this.inode = stat.ino;
    this.user_id = stat.uid;
    this.group_id = stat.gid;
    this.dev = stat.dev;
    this.atime_seconds = Math.floor(stat.atimeMs / 1000);
    this.mtime_seconds = Math.floor(stat.mtimeMs / 1000);
    this.ctime_seconds = Math.floor(stat.ctimeMs / 1000);
  }
}

/**
 * Perform some operation and return a Gleam `Result(a, String)`
 * where `a` is the type returned by the operation and the `String`
 * is the error code.
 *
 * @param {function():any} op
 * @returns {Ok | GError}
 */
function gleamResult(op) {
  try {
    const val = op();
    return new Ok(val);
  } catch (e) {
    return new GError(cast_error(e.code));
  }
}

function cast_error(error_code) {
  switch (error_code) {
    case "EACCES":
      return new $simplifile.Eacces();
    case "EAGAIN":
      return new $simplifile.Eagain();
    case "EBADF":
      return new $simplifile.Ebadf();
    case "EBADMSG":
      return new $simplifile.Ebadmsg();
    case "EBUSY":
      return new $simplifile.Ebusy();
    case "EDEADLK":
      return new $simplifile.Edeadlk();
    case "EDEADLOCK":
      return new $simplifile.Edeadlock();
    case "EDQUOT":
      return new $simplifile.Edquot();
    case "EEXIST":
      return new $simplifile.Eexist();
    case "EFAULT":
      return new $simplifile.Efault();
    case "EFBIG":
      return new $simplifile.Efbig();
    case "EFTYPE":
      return new $simplifile.Eftype();
    case "EINTR":
      return new $simplifile.Eintr();
    case "EINVAL":
      return new $simplifile.Einval();
    case "EIO":
      return new $simplifile.Eio();
    case "EISDIR":
      return new $simplifile.Eisdir();
    case "ELOOP":
      return new $simplifile.Eloop();
    case "EMFILE":
      return new $simplifile.Emfile();
    case "EMLINK":
      return new $simplifile.Emlink();
    case "EMULTIHOP":
      return new $simplifile.Emultihop();
    case "ENAMETOOLONG":
      return new $simplifile.Enametoolong();
    case "ENFILE":
      return new $simplifile.Enfile();
    case "ENOBUFS":
      return new $simplifile.Enobufs();
    case "ENODEV":
      return new $simplifile.Enodev();
    case "ENOLCK":
      return new $simplifile.Enolck();
    case "ENOLINK":
      return new $simplifile.Enolink();
    case "ENOENT":
      return new $simplifile.Enoent();
    case "ENOMEM":
      return new $simplifile.Enomem();
    case "ENOSPC":
      return new $simplifile.Enospc();
    case "ENOSR":
      return new $simplifile.Enosr();
    case "ENOSTR":
      return new $simplifile.Enostr();
    case "ENOSYS":
      return new $simplifile.Enosys();
    case "ENOBLK":
      return new $simplifile.Enotblk();
    case "ENODIR":
      return new $simplifile.Enotdir();
    case "ENOTSUP":
      return new $simplifile.Enotsup();
    case "ENXIO":
      return new $simplifile.Enxio();
    case "EOPNOTSUPP":
      return new $simplifile.Eopnotsupp();
    case "EOVERFLOW":
      return new $simplifile.Eoverflow();
    case "EPERM":
      return new $simplifile.Eperm();
    case "EPIPE":
      return new $simplifile.Epipe();
    case "ERANGE":
      return new $simplifile.Erange();
    case "EROFS":
      return new $simplifile.Erofs();
    case "ESPIPE":
      return new $simplifile.Espipe();
    case "ESRCH":
      return new $simplifile.Esrch();
    case "ESTALE":
      return new $simplifile.Estale();
    case "ETXTBSY":
      return new $simplifile.Etxtbsy();
    case "EXDEV":
      return new $simplifile.Exdev();
    case "NOTUTF8":
      return new $simplifile.NotUtf8();
    default:
      return new $simplifile.Unknown(error_code);
  }
}
