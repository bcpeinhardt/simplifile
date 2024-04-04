// @ts-check

import fs from "node:fs"
import path from "node:path"
import process from "node:process"
import { BitArray, Ok, Error as GError, toList} from "./gleam.mjs";
import { verify_is_directory } from "./simplifile.mjs";

/**
 * Read the contents of a file as a BitArray 
 * 
 * @param {string} filepath 
 * @returns {Ok | GError} A Result with the BitArray of the file contents
 */
export function readBits(filepath) {
    return gleamResult(() => {
        const contents = fs.readFileSync(path.normalize(filepath))
        return new BitArray(new Uint8Array(contents))
    })
}

/**
 * Write the given BitArray to a file
 * 
 * @param {BitArray} contents 
 * @param {string} filepath 
 * @returns {Ok | GError} 
 */
export function writeBits(contents, filepath) {
    return gleamResult(() => fs.writeFileSync(path.normalize(filepath), contents.buffer))
}

/**
 * Append the given BitArray to a file
 * 
 * @param {BitArray} contents 
 * @param {string} filepath 
 * @returns {Ok | GError}
 */
export function appendBits(contents, filepath) {
    return gleamResult(() => fs.appendFileSync(path.normalize(filepath), contents.buffer))
}

/**
 * Check whether a file exists at the given path
 * 
 * @deprecated Use `isValidFile` instead
 * @param {string} filepath 
 * @returns {boolean} 
 */
export function isFile(filepath) {
    let fp = path.normalize(filepath)
    return fs.existsSync(fp) && fs.statSync(fp).isFile();
}

/**
 * Check whether a file exists at the given path
 * 
 * @param {string} filepath 
 * @returns {Ok | GError} 
 */
export function isValidFile(filepath) {
    try {
        return new Ok(fs.statSync(path.normalize(filepath)).isFile());
    } catch (e) {
        if (e.code === 'ENOENT') {
            return new Ok(false);
        } else {
            return new GError(e.code);
        }
    }
}

/**
 * Check whether a symbolic link exists at the given path
 *
 * @param {string} filepath
 * @returns {Ok | GError}
 */
export function isValidSymlink(filepath) {
    try {
        return new Ok(fs.lstatSync(path.normalize(filepath)).isSymbolicLink());
    } catch (e) {
        if (e.code === 'ENOENT') {
            return new Ok(false);
        } else {
            return new GError(e.code);
        }
    }
}

/**
 * Check whether a directory exists at the given path
 * 
 * @deprecated Use `isValidDirectory` instead
 * @param {string} filepath 
 * @returns {boolean}
 */
export function isDirectory(filepath) {
    let fp = path.normalize(filepath)
    return fs.existsSync(fp) && fs.statSync(fp).isDirectory();
}

/**
 * Check whether a directory exists at the given path
 * 
 * @param {string} filepath 
 * @returns {Ok | GError} 
 */
export function isValidDirectory(filepath) {
    try {
        return new Ok(fs.statSync(path.normalize(filepath)).isDirectory());
    } catch (e) {
        if (e.code === 'ENOENT') {
            return new Ok(false);
        } else {
            return new GError(e.code);
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
export function makeSymlink(target, path) {
    return gleamResult(() => fs.symlinkSync(target, path))
}

/**
 * Create a directory at the given filepath
 * 
 * @param {string} filepath 
 * @returns {Ok | GError} 
 */
export function makeDirectory(filepath) {
   return gleamResult(() => fs.mkdirSync(path.normalize(filepath)))
}

/**
 * Recursively create a directory structure from the given path.
 * 
 * @param {string} filepath 
 * @returns 
 */
export function createDirAll(filepath) {
    return gleamResult(() => {
        fs.mkdirSync(path.normalize(filepath), { recursive: true })
    })
}

/**
 * Recursively delete a directory or delete a file at the given path.
 * 
 * @param {string} fileOrDirPath 
 * @returns {Ok | GError}
 */
export function deleteFileOrDirRecursive(fileOrDirPath) {
    return gleamResult(() => {
        const isDir = isValidDirectory(fileOrDirPath)
        if (isDir instanceof Ok && isDir[0] === true) {
            fs.rmSync(path.normalize(fileOrDirPath), { recursive: true })
        } else {
            fs.unlinkSync(path.normalize(fileOrDirPath))
        }
    })
}

/**
 * List the contents of a directory.
 * 
 * @param {string} filepath 
 * @returns {Ok | GError}
 */
export function listContents(filepath) {
    return gleamResult(() => toList(fs.readdirSync(path.normalize(filepath))))
}

/**
 * Copy a file to a new path.
 * 
 * @param {string} srcpath 
 * @param {string} destpath 
 * @returns {Ok | GError}
 */
export function copyFile(srcpath, destpath) {
    return gleamResult(() => fs.copyFileSync(path.normalize(srcpath), path.normalize(destpath)))
}

/**
 * Move a file to the new path.
 * 
 * @param {string} srcpath 
 * @param {string} destpath 
 * @returns {Ok | GError}
 */
export function renameFile(srcpath, destpath) {
    return gleamResult(() => fs.renameSync(path.normalize(srcpath), path.normalize(destpath)))
}

/**
 * Set the file permissions. Octal number should be in base 8.
 * 
 * @param {string} filepath 
 * @param {number} octalNumber 
 * @returns {Ok | GError}
 */
export function setPermissions(filepath, octalNumber) {
    return gleamResult(() => fs.chmodSync(path.normalize(filepath), octalNumber))
}

/**
 * Return the current directory.
 * 
 * @returns {Ok | GError} The current directory
 */
export function currentDirectory() {
    return gleamResult(() => process.cwd())
}

/**
 * 
 * @param {string} filepath 
 * @returns {Ok | GError}
 */
export function fileInfo(filepath) {
    return gleamResult(() => new FileInfo(filepath))
}

class FileInfo {
    constructor(filepath) {
        const stat = fs.statSync(path.normalize(filepath))
        this.size = stat.size
        this.mode = stat.mode
        this.nlinks = stat.nlink
        this.inode = stat.ino
        this.userId = stat.uid
        this.groupId = stat.gid
        this.dev = stat.dev
        this.atime = Math.floor(stat.atimeMs / 1000)
        this.mtime = Math.floor(stat.mtimeMs / 1000)
        this.ctime = Math.floor(stat.ctimeMs / 1000)
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
        const val = op()
        return new Ok(val)
    } catch(e) {
        return new GError(e.code)
    }
}