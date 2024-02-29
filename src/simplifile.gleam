import gleam/bit_array
import gleam/string
import gleam/result
import gleam/list
import gleam/set.{type Set}
import gleam/int

/// This type represents all of the reasons for why a file system operation could fail.
///
/// Most of these reasons are POSIX errors, which come from the operating system
/// and start with E. Others have been added to represent other issues that may
/// arise specific to this library.
///
pub type FileError {
  /// Permission denied.
  Eacces
  /// Resource temporarily unavailable.
  Eagain
  /// Bad file number
  Ebadf
  /// Bad message.
  Ebadmsg
  /// File busy.
  Ebusy
  /// Resource deadlock avoided.
  Edeadlk
  /// On most architectures, same as `Edeadlk`. On some architectures, it
  /// means "File locking deadlock error."
  Edeadlock
  /// Disk quota exceeded.
  Edquot
  /// File already exists.
  Eexist
  /// Bad address in system call argument.
  Efault
  /// File too large.
  Efbig
  /// Inappropriate file type or format. Usually caused by trying to set the
  /// "sticky bit" on a regular file (not a directory).
  Eftype
  /// Interrupted system call.
  Eintr
  /// Invalid argument.
  Einval
  /// I/O error.
  Eio
  /// Illegal operation on a directory.
  Eisdir
  /// Too many levels of symbolic links.
  Eloop
  /// Too many open files.
  Emfile
  /// Too many links.
  Emlink
  /// Multihop attempted.
  Emultihop
  /// Filename too long
  Enametoolong
  /// File table overflow
  Enfile
  /// No buffer space available.
  Enobufs
  /// No such device.
  Enodev
  /// No locks available.
  Enolck
  /// Link has been severed.
  Enolink
  /// No such file or directory.
  Enoent
  /// Not enough memory.
  Enomem
  /// No space left on device.
  Enospc
  /// No STREAM resources.
  Enosr
  /// Not a STREAM.
  Enostr
  /// Function not implemented.
  Enosys
  /// Block device required.
  Enotblk
  /// Not a directory.
  Enotdir
  /// Operation not supported.
  Enotsup
  /// No such device or address.
  Enxio
  /// Operation not supported on socket.
  Eopnotsupp
  /// Value too large to be stored in data type.
  Eoverflow
  /// Not owner.
  Eperm
  /// Broken pipe.
  Epipe
  /// Result too large.
  Erange
  /// Read-only file system.
  Erofs
  /// Invalid seek.
  Espipe
  /// No such process.
  Esrch
  /// Stale remote file handle.
  Estale
  /// Text file busy.
  Etxtbsy
  /// Cross-domain link.
  Exdev
  /// File was requested to be read as UTF-8, but is not UTF-8 encoded.
  NotUtf8
  /// Any error not accounted for by this type
  Unknown
}

/// Represents the intersection of information available
/// from erlang's `file:read_file_info` and node's `fs.stat`
pub type FileInfo {
  FileInfo(
    /// File size in bytes.
    size: Int,
    /// File mode that indicates the file type and its permissions. 
    /// For example, in Unix and Linux, a mode value of 33188 indicates 
    /// a regular file and the permissions associated with it 
    /// (read and write for the owner, and read-only for others, in 
    /// this case).
    mode: Int,
    /// Number of hard links that exist for the file.
    nlinks: Int,
    /// Inode number, which is a unique identifier for the file in the filesystem.
    inode: Int,
    /// User ID of the file's owner.
    user_id: Int,
    /// Group ID of the file's group.
    group_id: Int,
    /// Device ID of the file's major device.
    /// TODO: We can actually get a major device and minor device from both
    /// node and erlang. The `fs.stat` in node returns a `dev` and `rdev`,
    /// so we can use some bitwise operations to get the minor out of `rdev`.
    /// Someone who's not me should totally make a PR for that.
    dev: Int,
    /// The last access time in seconds since the UNIX epoch (00:00:00 UTC on 1 January 1970).
    atime_seconds: Int,
    /// The last modification time in seconds since the UNIX epoch (00:00:00 UTC on 1 January 1970).
    mtime_seconds: Int,
    /// The last change time in seconds since the UNIX epoch (00:00:00 UTC on 1 January 1970).
    ctime_seconds: Int,
  )
}

@target(erlang)
@external(erlang, "simplifile_erl", "file_info")
fn do_file_info(a: String) -> Result(FileInfo, FileError)

@target(javascript)
@external(javascript, "./simplifile_js.mjs", "fileInfo")
fn do_file_info(a: String) -> Result(FileInfo, String)

/// Get information about a file at a given path
pub fn file_info(a: String) -> Result(FileInfo, FileError) {
  do_file_info(a)
  |> cast_error
}

/// Read a files contents as a string
/// ## Example
/// ```gleam
/// let assert Ok(records) = read(from: "./users.csv")
/// ```
///
pub fn read(from filepath: String) -> Result(String, FileError) {
  do_read(filepath)
  |> cast_error
}

/// Write a string to a file at the given path
/// ## Example
/// ```gleam
/// let assert Ok(Nil) = write("Hello, World!", to: "./hello_world.txt")
/// ```
///
pub fn write(
  to filepath: String,
  contents contents: String,
) -> Result(Nil, FileError) {
  do_write(contents, to: filepath)
  |> cast_error
}

/// Delete a file or directory at a given path. Performs a recursive
/// delete on a directory.
/// Throws an error if the path does not exist.
/// ## Example
/// ```gleam
/// let assert Ok(Nil) = delete(file_at: "./delete_me.txt")
/// ```
///
pub fn delete(file_or_dir_at path: String) -> Result(Nil, FileError) {
  do_delete(path)
  |> cast_error
}

/// Delete all files/directories specified in a list of paths.
/// Recursively deletes provided directories.
/// Does not return an error if one or more of the provided paths 
/// do not exist. 
/// 
pub fn delete_all(paths paths: List(String)) -> Result(Nil, FileError) {
  case paths {
    [] -> Ok(Nil)
    [path, ..rest] -> {
      case delete(path) {
        Ok(Nil) | Error(Enoent) -> delete_all(rest)
        e -> e
      }
    }
  }
}

/// Append a string to the contents of a file at the given path
/// ## Example
/// ```gleam
/// let assert Ok(Nil) = append("more text", to: "./needs_more_text.txt")
/// ```
///
pub fn append(
  to filepath: String,
  contents contents: String,
) -> Result(Nil, FileError) {
  do_append(contents, to: filepath)
  |> cast_error
}

/// Read a files contents as a bitstring
/// ## Example
/// ```gleam
/// let assert Ok(records) = read_bits(from: "./users.csv")
/// ```
///
pub fn read_bits(from filepath: String) -> Result(BitArray, FileError) {
  do_read_bits(filepath)
  |> cast_error
}

/// Write a bitstring to a file at the given path
/// ## Example
/// ```gleam
/// let assert Ok(Nil) = write_bits(<<"Hello, World!":utf8>>, to: "./hello_world.txt")
/// ```
///
pub fn write_bits(
  to filepath: String,
  bits bits: BitArray,
) -> Result(Nil, FileError) {
  do_write_bits(bits, filepath)
  |> cast_error
}

/// Append a bitstring to the contents of a file at the given path
/// ## Example
/// ```gleam
/// let assert Ok(Nil) = append_bits(<<"more text":utf8>>, to: "./needs_more_text.txt")
/// ```
///
pub fn append_bits(
  to filepath: String,
  bits bits: BitArray,
) -> Result(Nil, FileError) {
  do_append_bits(bits, filepath)
  |> cast_error
}

/// Checks if the provided filepath is a directory
/// ## Example
/// ```gleam
/// let assert True = is_directory("./test")
/// ```
/// 
@deprecated("Use `verify_is_directory` instead")
pub fn is_directory(filepath: String) -> Bool {
  do_is_directory(filepath)
}

/// Checks if the provided filepath exists and is a directory.
/// Returns an error if it lacks permissions to read the directory.
/// 
/// ## Example
/// ```gleam
/// let assert Ok(True) = verify_is_directory("./test")
/// ```
pub fn verify_is_directory(filepath: String) -> Result(Bool, FileError) {
  do_verify_is_directory(filepath)
  |> cast_error
}

@target(javascript)
@external(javascript, "./simplifile_js.mjs", "isValidDirectory")
fn do_verify_is_directory(filepath: String) -> Result(Bool, String)

@target(erlang)
@external(erlang, "simplifile_erl", "is_valid_directory")
fn do_verify_is_directory(filepath: String) -> Result(Bool, FileError)

/// Create a directory at the provided filepath. Returns an error if
/// the directory already exists.
///
/// ## Example
/// ```gleam
/// create_directory("./test")
/// ```
pub fn create_directory(filepath: String) -> Result(Nil, FileError) {
  do_make_directory(filepath)
  |> cast_error
}

/// Lists the contents of a directory.
/// The list contains directory and file names, and is not recursive.
/// 
/// ## Example
/// ```gleam
/// let assert Ok(files_and_folders) = read_directory(at: "./Folder1")
/// ```
/// 
pub fn read_directory(at path: String) -> Result(List(String), FileError) {
  do_read_directory(path)
  |> cast_error
}

/// Returns `True` if there is a file at the given path, false otherwise.
/// 
@deprecated("Use `verify_is_file` instead")
pub fn is_file(filepath: String) -> Bool {
  do_is_file(filepath)
}

/// Checks if the file at the provided filepath exists and is a file.
/// Returns an Error if it lacks permissions to read the file.
/// 
/// ## Example
/// ```gleam
/// let assert Ok(True) = verify_is_file("./test.txt")
/// ```
///
pub fn verify_is_file(filepath: String) -> Result(Bool, FileError) {
  do_verify_is_file(filepath)
  |> cast_error
}

@target(javascript)
@external(javascript, "./simplifile_js.mjs", "isValidFile")
fn do_verify_is_file(filepath: String) -> Result(Bool, String)

@target(erlang)
@external(erlang, "simplifile_erl", "is_valid_file")
fn do_verify_is_file(filepath: String) -> Result(Bool, FileError)

/// Creates an empty file at the given filepath. Returns an `Error(Eexist)`
/// if the file already exists.
/// 
pub fn create_file(at filepath: String) -> Result(Nil, FileError) {
  case
    filepath
    |> verify_is_file,
    filepath
    |> verify_is_directory
  {
    Ok(True), _ | _, Ok(True) -> Error(Eexist)
    _, _ -> write_bits(<<>>, to: filepath)
  }
}

/// Recursively creates necessary directories for a given directory
/// path. Note that if you pass a path that "looks like" a file, i.e.
/// `./a/b.txt`, a folder named `b.txt` will be created, so be sure
/// to pass only the path to the required directory.
pub fn create_directory_all(dirpath: String) -> Result(Nil, FileError) {
  let path = case
    dirpath
    |> string.ends_with("/")
  {
    True -> dirpath
    False -> dirpath <> "/"
  }
  do_create_dir_all(path)
  |> cast_error
}

/// Copy a file at a given path to another path.
/// Note: destination should include the filename, not just the directory
pub fn copy_file(at src: String, to dest: String) -> Result(Nil, FileError) {
  do_copy_file(src, dest)
  |> result.replace(Nil)
  |> cast_error
}

/// Rename a file at a given path to another path.
/// Note: destination should include the filename, not just the directory
pub fn rename_file(at src: String, to dest: String) -> Result(Nil, FileError) {
  do_rename_file(src, dest)
  |> cast_error
}

/// Copy a directory recursively
pub fn copy_directory(at src: String, to dest: String) -> Result(Nil, FileError) {
  // Erlang does not provide a built in `copy_dir` function, 
  // and Deno doesn't support Node's `fs.cpSync`, so we'll just roll 
  // our own for now.
  use _ <- result.try(create_directory_all(dest))
  do_copy_directory(src, dest)
}

fn do_copy_directory(src: String, dest: String) -> Result(Nil, FileError) {
  // Iterate over the segments of the file
  use segments <- result.try(read_directory(src))
  segments
  |> list.each(fn(segment) {
    let src_path = src <> "/" <> segment
    let dest_path = dest <> "/" <> segment

    case verify_is_file(src_path), verify_is_directory(src_path) {
      Ok(True), Ok(False) -> {
        // For a file, create the file in the new directory
        use content <- result.try(read_bits(src_path))
        content
        |> write_bits(to: dest_path)
      }
      Ok(False), Ok(True) -> {
        // Create the target directory and recurse
        use _ <- result.try(create_directory(dest_path))
        do_copy_directory(src_path, dest_path)
      }
      Error(e), _ | _, Error(e) -> {
        Error(e)
      }
      Ok(False), Ok(False) | Ok(True), Ok(True) -> {
        // We're really not sure how that one happened.
        Error(Unknown)
      }
    }
  })
  Ok(Nil)
}

/// Copy a directory recursively and then delete the old one.
pub fn rename_directory(
  at src: String,
  to dest: String,
) -> Result(Nil, FileError) {
  use _ <- result.try(copy_directory(src, dest))
  delete(src)
}

/// Returns a list of filepaths for every file in the directory, including nested
/// files.
/// 
pub fn get_files(in directory: String) -> Result(List(String), FileError) {
  use contents <- result.try(read_directory(directory))
  let paths =
    list.map(contents, fn(segment) {
      case
        directory
        |> string.ends_with("/")
      {
        True -> directory <> segment
        False -> directory <> "/" <> segment
      }
    })
  let files = list.filter(paths, fn(path) { verify_is_file(path) == Ok(True) })
  case list.filter(paths, fn(path) { verify_is_directory(path) == Ok(True) }) {
    [] -> Ok(files)
    directories -> {
      use nested_files <- result.try(list.try_map(directories, get_files))
      Ok(list.append(files, list.flatten(nested_files)))
    }
  }
}

/// Represents a file permission
pub type Permission {
  Read
  Write
  Execute
}

fn permission_to_integer(permission: Permission) -> Int {
  case permission {
    Read -> 0o4
    Write -> 0o2
    Execute -> 0o1
  }
}

/// Represents a set of file permissions for a given file
pub type FilePermissions {
  FilePermissions(
    user: Set(Permission),
    group: Set(Permission),
    other: Set(Permission),
  )
}

pub fn file_permissions_to_octal(permissions: FilePermissions) -> Int {
  let make_permission_digit = fn(permissions: Set(Permission)) {
    permissions
    |> set.to_list
    |> list.map(permission_to_integer)
    |> int.sum
  }

  make_permission_digit(permissions.user)
  * 64
  + make_permission_digit(permissions.group)
  * 8
  + make_permission_digit(permissions.other)
}

/// Sets the permissions for a given file
/// 
/// # Example
/// ```gleam
/// let all = set.from_list([Read, Write, Execute])
/// let all = FilePermissions(user: all, group: all, other: all)
/// let assert Ok(Nil) = set_permissions("./script.sh", all)
/// ```
pub fn set_permissions(
  for_file_at filepath: String,
  to permissions: FilePermissions,
) -> Result(Nil, FileError) {
  set_permissions_octal(filepath, file_permissions_to_octal(permissions))
}

/// Sets the permissions for a given file using an octal representation
/// 
/// # Example
/// ```gleam
/// set_permissions_octal("./script.sh", 0o777)
/// ```
pub fn set_permissions_octal(
  for_file_at filepath: String,
  to permissions: Int,
) -> Result(Nil, FileError) {
  do_set_permissions(filepath, permissions)
  |> cast_error
}

/// Returns the current working directory
/// 
pub fn current_directory() -> Result(String, FileError) {
  do_current_directory()
  |> cast_error
}

@target(javascript)
@external(javascript, "./simplifile_js.mjs", "currentDirectory")
fn do_current_directory() -> Result(String, String)

@target(erlang)
fn do_current_directory() -> Result(String, FileError) {
  do_do_current_directory()
  |> result.map(string.from_utf_codepoints)
}

@target(erlang)
@external(erlang, "file", "get_cwd")
fn do_do_current_directory() -> Result(List(UtfCodepoint), FileError)

@target(javascript)
@external(javascript, "./simplifile_js.mjs", "setPermissions")
fn do_set_permissions(
  file_at: String,
  permissions_octal: Int,
) -> Result(Nil, String)

@target(erlang)
@external(erlang, "simplifile_erl", "set_permissions")
fn do_set_permissions(
  file_at: String,
  permissions_octal: Int,
) -> Result(Nil, FileError)

@target(javascript)
fn do_read(from filepath: String) -> Result(String, String) {
  case do_read_bits(filepath) {
    Ok(bits) -> {
      case bit_array.to_string(bits) {
        Ok(str) -> Ok(str)
        _ -> Error("NOTUTF8")
      }
    }
    Error(e) -> Error(e)
  }
}

@target(javascript)
fn do_write(content: String, to filepath: String) -> Result(Nil, String) {
  content
  |> bit_array.from_string
  |> do_write_bits(to: filepath)
}

@target(javascript)
@external(javascript, "./simplifile_js.mjs", "deleteFileOrDirRecursive")
fn do_delete(file_or_dir_at: String) -> Result(Nil, String)

@target(javascript)
fn do_append(content: String, to filepath: String) -> Result(Nil, String) {
  content
  |> bit_array.from_string
  |> do_append_bits(to: filepath)
}

@target(javascript)
@external(javascript, "./simplifile_js.mjs", "readBits")
fn do_read_bits(from: String) -> Result(BitArray, String)

@target(javascript)
@external(javascript, "./simplifile_js.mjs", "writeBits")
fn do_write_bits(content: BitArray, to filepath: String) -> Result(Nil, String)

@target(javascript)
@external(javascript, "./simplifile_js.mjs", "appendBits")
fn do_append_bits(content: BitArray, to filepath: String) -> Result(Nil, String)

@target(javascript)
@external(javascript, "./simplifile_js.mjs", "isDirectory")
fn do_is_directory(filepath: String) -> Bool

@target(javascript)
@external(javascript, "./simplifile_js.mjs", "makeDirectory")
fn do_make_directory(filepath: String) -> Result(Nil, String)

@target(javascript)
@external(javascript, "./simplifile_js.mjs", "createDirAll")
fn do_create_dir_all(dirpath: String) -> Result(Nil, String)

@target(javascript)
@external(javascript, "./simplifile_js.mjs", "listContents")
fn do_read_directory(directory_path: String) -> Result(List(String), String)

@target(javascript)
@external(javascript, "./simplifile_js.mjs", "copyFile")
fn do_copy_file(at: String, to: String) -> Result(Nil, String)

@target(javascript)
@external(javascript, "./simplifile_js.mjs", "renameFile")
fn do_rename_file(at: String, to: String) -> Result(Nil, String)

@target(javascript)
fn cast_error(input: Result(a, String)) -> Result(a, FileError) {
  result.map_error(input, fn(e) {
    case e {
      "EACCES" -> Eacces
      "EAGAIN" -> Eagain
      "EBADF" -> Ebadf
      "EBADMSG" -> Ebadmsg
      "EBUSY" -> Ebusy
      "EDEADLK" -> Edeadlk
      "EDEADLOCK" -> Edeadlock
      "EDQUOT" -> Edquot
      "EEXIST" -> Eexist
      "EFAULT" -> Efault
      "EFBIG" -> Efbig
      "EFTYPE" -> Eftype
      "EINTR" -> Eintr
      "EINVAL" -> Einval
      "EIO" -> Eio
      "EISDIR" -> Eisdir
      "ELOOP" -> Eloop
      "EMFILE" -> Emfile
      "EMLINK" -> Emlink
      "EMULTIHOP" -> Emultihop
      "ENAMETOOLONG" -> Enametoolong
      "ENFILE" -> Enfile
      "ENOBUFS" -> Enobufs
      "ENODEV" -> Enodev
      "ENOLCK" -> Enolck
      "ENOLINK" -> Enolink
      "ENOENT" -> Enoent
      "ENOMEM" -> Enomem
      "ENOSPC" -> Enospc
      "ENOSR" -> Enosr
      "ENOSTR" -> Enostr
      "ENOSYS" -> Enosys
      "ENOBLK" -> Enotblk
      "ENODIR" -> Enotdir
      "ENOTSUP" -> Enotsup
      "ENXIO" -> Enxio
      "EOPNOTSUPP" -> Eopnotsupp
      "EOVERFLOW" -> Eoverflow
      "EPERM" -> Eperm
      "EPIPE" -> Epipe
      "ERANGE" -> Erange
      "EROFS" -> Erofs
      "ESPIPE" -> Espipe
      "ESRCH" -> Esrch
      "ESTALE" -> Estale
      "ETXTBSY" -> Etxtbsy
      "EXDEV" -> Exdev
      "NOTUTF8" -> NotUtf8
      _ -> Unknown
    }
  })
}

@target(erlang)
@external(erlang, "simplifile_erl", "append_file")
fn do_append_bits(
  content: BitArray,
  to filepath: String,
) -> Result(Nil, FileError)

@target(erlang)
@external(erlang, "simplifile_erl", "write_file")
fn do_write_bits(
  content: BitArray,
  to filepath: String,
) -> Result(Nil, FileError)

@target(erlang)
@external(erlang, "simplifile_erl", "read_file")
fn do_read_bits(from: String) -> Result(BitArray, FileError)

@target(erlang)
@external(erlang, "simplifile_erl", "recursive_delete")
fn do_delete(file_or_dir_at: String) -> Result(Nil, FileError)

@target(erlang)
fn do_append(content: String, to filepath: String) -> Result(Nil, FileError) {
  content
  |> bit_array.from_string
  |> do_append_bits(filepath)
}

@target(erlang)
fn do_write(content: String, to filepath: String) -> Result(Nil, FileError) {
  content
  |> bit_array.from_string
  |> do_write_bits(filepath)
}

@target(erlang)
fn do_read(from filepath: String) -> Result(String, FileError) {
  case do_read_bits(filepath) {
    Ok(bits) -> {
      case bit_array.to_string(bits) {
        Ok(str) -> Ok(str)
        _ -> Error(NotUtf8)
      }
    }
    Error(e) -> Error(e)
  }
}

@target(erlang)
fn cast_error(input: Result(a, FileError)) -> Result(a, FileError) {
  input
}

@target(erlang)
@external(erlang, "filelib", "is_dir")
fn do_is_directory(path: String) -> Bool

@target(erlang)
@external(erlang, "simplifile_erl", "make_directory")
fn do_make_directory(directory: String) -> Result(Nil, FileError)

@target(erlang)
@external(erlang, "simplifile_erl", "list_directory")
fn do_read_directory(directory: String) -> Result(List(String), FileError)

@external(erlang, "simplifile_erl", "is_file")
@external(javascript, "./simplifile_js.mjs", "isFile")
fn do_is_file(filepath: String) -> Bool

@target(erlang)
@external(erlang, "simplifile_erl", "create_dir_all")
fn do_create_dir_all(dirpath: String) -> Result(Nil, FileError)

@target(erlang)
@external(erlang, "file", "copy")
fn do_copy_file(src: String, dest: String) -> Result(Int, FileError)

@target(erlang)
@external(erlang, "simplifile_erl", "rename_file")
fn do_rename_file(src: String, dest: String) -> Result(Nil, FileError)
