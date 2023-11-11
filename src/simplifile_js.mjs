import fs from "node:fs"
import path from "node:path"
import { BitArray, Ok, Error as GError, toList} from "./gleam.mjs";

export function readBits(filepath) {
    try {
        const contents = fs.readFileSync(path.normalize(filepath))
        return new Ok(new BitArray(new Uint8Array(contents)))
    } catch(e) {
        return new GError(stringifyError(e))
    }
}

export function writeBits(contents, filepath) {
    try {
        fs.writeFileSync(path.normalize(filepath), contents.buffer)
        return new Ok(undefined)
    } catch (e) {
        return new GError(stringifyError(e))
    }
}

export function appendBits(contents, filepath) {
    try {
        fs.appendFileSync(path.normalize(filepath), contents.buffer)
        return new Ok(undefined)
    } catch (e) {
        return new GError(stringifyError(e))
    }
}

function stringifyError(e) {
    return e.code
}

export function isFile(filepath) {
    let fp = path.normalize(filepath)
    return fs.existsSync(fp) && fs.lstatSync(fp).isFile();
}

export function isDirectory(filepath) {
    let fp = path.normalize(filepath)
    return fs.existsSync(fp) && fs.lstatSync(fp).isDirectory();
}

export function makeDirectory(filepath) {
    try {
        fs.mkdirSync(path.normalize(filepath))
        return new Ok(undefined)
    } catch (e) {
        return new GError(stringifyError(e))
    }
}

export function createDirAll(filepath) {
    try {
        fs.mkdirSync(filepath, { recursive: true })
        return new Ok(undefined)
    } catch (e) {
        return new GError(stringifyError(e))
    }
}

export function deleteFileOrDirRecursive(fileOrDirPath) {
    try {
        if (isDirectory(fileOrDirPath)) {
            fs.rmSync(path.normalize(fileOrDirPath), { recursive: true })
        } else {
            fs.unlinkSync(path.normalize(fileOrDirPath))
        }
        return new Ok(undefined)
    } catch (e) {
        return new GError(stringifyError(e))
    }
}

export function listContents(filepath) {
    try {
        const stuff = toList(fs.readdirSync(path.normalize(filepath)))
        return new Ok(stuff)
    } catch(e) {
        return new GError(stringifyError(e))
    }
}

export function copyFile(srcpath, destpath) {
    try {
        fs.copyFileSync(path.normalize(srcpath), path.normalize(destpath))
        return new Ok(undefined)
    } catch (e) {
        return new GError(stringifyError(e))
    }
}

export function renameFile(srcpath, destpath) {
    try {
        fs.renameSync(path.normalize(srcpath), path.normalize(destpath))
        return new Ok(undefined)
    } catch (e) {
        return new GError(stringifyError(e))
    }
}