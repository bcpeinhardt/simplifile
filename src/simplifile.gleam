import filepath
import gleam/bit_array
import gleam/int
import gleam/list
import gleam/result
import gleam/set.{type Set}
import gleam/string

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
  Unknown(inner: String)
}

/// Convert an error into a human-readable description
/// ## Example
/// ```gleam
/// let assert "Input/output error" = describe_error(Eio)
/// ```
///
pub fn describe_error(error: FileError) -> String {
  case error {
    Eperm -> "Operation not permitted"
    Enoent -> "No such file or directory"
    Esrch -> "No such process"
    Eintr -> "Interrupted system call"
    Eio -> "Input/output error"
    Enxio -> "Device not configured"
    Ebadf -> "Bad file descriptor"
    Edeadlk -> "Resource deadlock avoided"
    Edeadlock -> "Resource deadlock avoided"
    Enomem -> "Cannot allocate memory"
    Eacces -> "Permission denied"
    Efault -> "Bad address"
    Enotblk -> "Block device required"
    Ebusy -> "Resource busy"
    Eexist -> "File exists"
    Exdev -> "Cross-device link"
    Enodev -> "Operation not supported by device"
    Enotdir -> "Not a directory"
    Eisdir -> "Is a directory"
    Einval -> "Invalid argument"
    Enfile -> "Too many open files in system"
    Emfile -> "Too many open files"
    Etxtbsy -> "Text file busy"
    Efbig -> "File too large"
    Enospc -> "No space left on device"
    Espipe -> "Illegal seek"
    Erofs -> "Read-only file system"
    Emlink -> "Too many links"
    Epipe -> "Broken pipe"
    Erange -> "Result too large"
    Eagain -> "Resource temporarily unavailable"
    Enotsup -> "Operation not supported"
    Enobufs -> "No buffer space available"
    Eloop -> "Too many levels of symbolic links"
    Enametoolong -> "File name too long"
    Edquot -> "Disc quota exceeded"
    Estale -> "Stale NFS file handle"
    Enolck -> "No locks available"
    Enosys -> "Function not implemented"
    Eftype -> "Inappropriate file type or format"
    Eoverflow -> "Value too large to be stored in data type"
    Ebadmsg -> "Bad message"
    Emultihop -> "Multihop attempted"
    Enolink -> "Link has been severed"
    Enosr -> "No STREAM resources"
    Enostr -> "Not a STREAM"
    Eopnotsupp -> "Operation not supported on socket"
    NotUtf8 -> "File not UTF-8 encoded"
    Unknown(inner) -> "Unknown error: " <> inner
  }
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

/// Extract the file permissions from a given FileInfo value in their octal representation.
///
/// ## Example
/// ```gleam
/// use info <- result.try(simplifile.file_info("src/app.gleam"))
/// simplifile.file_info_permissions_octal(info)
/// // --> 0o644
/// ```
pub fn file_info_permissions_octal(from file_info: FileInfo) -> Int {
  int.bitwise_and(file_info.mode, 0o777)
}

/// Extract the `FilePermissions` from a given FileInfo value.
pub fn file_info_permissions(from file_info: FileInfo) -> FilePermissions {
  octal_to_file_permissions(file_info_permissions_octal(file_info))
}

/// An enumeration of different types of files.
pub type FileType {
  /// A regular file
  File
  /// A directory
  Directory
  /// A symbolic link
  Symlink
  /// Another special file type present on some systems, lika a socket or device
  Other
}

/// Extract the file type from a given FileInfo value.
///
/// ## Example
/// ```gleam
/// use info <- result.try(simplifile.file_info("src/app.gleam"))
/// simplifile.file_info_type(info)
/// // --> simplifile.File
/// ```
pub fn file_info_type(from file_info: FileInfo) -> FileType {
  // S_IFMT and related constants;
  // see https://www.man7.org/linux/man-pages/man7/inode.7.html
  // see https://github.com/nodejs/node/blob/main/typings/internalBinding/constants.d.ts#L147
  case int.bitwise_and(file_info.mode, 0o170000) {
    0o100000 -> File
    0o40000 -> Directory
    0o120000 -> Symlink
    _ -> Other
  }
}

/// Get information about a file at a given path
///
/// When the given `filepath` points to a symlink, this function will follow
/// the symlink and return information about the target file.
///
/// See `link_info` if you want to get information about a symlink instead.
@external(erlang, "simplifile_erl", "file_info")
@external(javascript, "./simplifile_js.mjs", "fileInfo")
pub fn file_info(filepath: String) -> Result(FileInfo, FileError)

/// Get information about a file at a given path
///
/// When the given `filepath` is a symlink, this function will return
/// infromation about the symlink itself.
///
/// See `file_info` if you want to follow symlinks instead.
@external(erlang, "simplifile_erl", "link_info")
@external(javascript, "./simplifile_js.mjs", "linkInfo")
pub fn link_info(filepath: String) -> Result(FileInfo, FileError)

/// Read a files contents as a string
/// ## Example
/// ```gleam
/// let assert Ok(records) = read(from: "./users.csv")
/// ```
///
pub fn read(from filepath: String) -> Result(String, FileError) {
  case read_bits(filepath) {
    Ok(bits) -> {
      case bit_array.to_string(bits) {
        Ok(str) -> Ok(str)
        _ -> Error(NotUtf8)
      }
    }
    Error(e) -> Error(e)
  }
}

/// Write a string to a file at the given path
/// ## Example
/// ```gleam
/// let assert Ok(Nil) = write(to: "./hello_world.txt", contents: "Hello, World!")
/// ```
///
pub fn write(
  to filepath: String,
  contents contents: String,
) -> Result(Nil, FileError) {
  contents
  |> bit_array.from_string
  |> write_bits(to: filepath)
}

/// Delete a file or directory at a given path. Performs a recursive
/// delete on a directory.
/// Throws an error if the path does not exist.
/// ## Example
/// ```gleam
/// let assert Ok(Nil) = delete(file_at: "./delete_me.txt")
/// ```
@external(erlang, "simplifile_erl", "delete")
@external(javascript, "./simplifile_js.mjs", "delete_")
pub fn delete(file_or_dir_at path: String) -> Result(Nil, FileError)

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
/// let assert Ok(Nil) = append(to: "./needs_more_text.txt", contents: "more text")
/// ```
///
pub fn append(
  to filepath: String,
  contents contents: String,
) -> Result(Nil, FileError) {
  contents
  |> bit_array.from_string
  |> append_bits(to: filepath)
}

/// Read a files contents as a bitstring
/// ## Example
/// ```gleam
/// let assert Ok(records) = read_bits(from: "./users.csv")
/// ```
@external(erlang, "simplifile_erl", "read_bits")
@external(javascript, "./simplifile_js.mjs", "readBits")
pub fn read_bits(from filepath: String) -> Result(BitArray, FileError)

/// Write a bitstring to a file at the given path
/// ## Example
/// ```gleam
/// let assert Ok(Nil) = write_bits(to: "./hello_world.txt", bits: <<"Hello, World!":utf8>>)
/// ```
///
@external(erlang, "simplifile_erl", "write_bits")
@external(javascript, "./simplifile_js.mjs", "writeBits")
pub fn write_bits(
  to filepath: String,
  bits bits: BitArray,
) -> Result(Nil, FileError)

/// Append a bitstring to the contents of a file at the given path
/// ## Example
/// ```gleam
/// let assert Ok(Nil) = append_bits(to: "./needs_more_text.txt", bits: <<"more text":utf8>>)
/// ```
///
@external(erlang, "simplifile_erl", "append_bits")
@external(javascript, "./simplifile_js.mjs", "appendBits")
pub fn append_bits(
  to filepath: String,
  bits bits: BitArray,
) -> Result(Nil, FileError)

/// Checks if the provided filepath exists and is a directory.
/// Returns an error if it lacks permissions to read the directory.
///
/// ## Example
/// ```gleam
/// let assert Ok(True) = is_directory("./test")
/// ```
@external(erlang, "simplifile_erl", "is_directory")
@external(javascript, "./simplifile_js.mjs", "isDirectory")
pub fn is_directory(filepath: String) -> Result(Bool, FileError)

/// Create a directory at the provided filepath. Returns an error if
/// the directory already exists.
///
/// ## Example
/// ```gleam
/// create_directory("./test")
/// ```
@external(erlang, "simplifile_erl", "create_directory")
@external(javascript, "./simplifile_js.mjs", "createDirectory")
pub fn create_directory(filepath: String) -> Result(Nil, FileError)

/// Create a symbolic link called symlink pointing to target.
/// 
/// ### Footgun Alert 
/// the target path is relative to *the symlink*,
/// not the current working directory. I will likely be updating 
/// the label on the next major version to reflect that.
///
/// ## Example
/// ```gleam
/// create_symlink("../target", "./symlink")
/// ```
@external(erlang, "simplifile_erl", "create_symlink")
@external(javascript, "./simplifile_js.mjs", "createSymlink")
pub fn create_symlink(
  to target: String,
  from symlink: String,
) -> Result(Nil, FileError)

/// Lists the contents of a directory.
/// The list contains directory and file names, and is not recursive.
///
/// ## Example
/// ```gleam
/// let assert Ok(files_and_folders) = read_directory(at: "./Folder1")
/// ```
///
@external(erlang, "simplifile_erl", "read_directory")
@external(javascript, "./simplifile_js.mjs", "readDirectory")
pub fn read_directory(at path: String) -> Result(List(String), FileError)

/// Checks if the file at the provided filepath exists and is a file.
/// Returns an Error if it lacks permissions to read the file.
///
/// ## Example
/// ```gleam
/// let assert Ok(True) = is_file("./test.txt")
/// ```
///
@external(erlang, "simplifile_erl", "is_file")
@external(javascript, "./simplifile_js.mjs", "isFile")
pub fn is_file(filepath: String) -> Result(Bool, FileError)

/// Checks if the file at the provided filepath exists and is a symbolic link.
/// Returns an Error if it lacks permissions to read the file.
///
/// ## Example
/// ```gleam
/// let assert Ok(True) = is_symlink("./symlink")
/// ```
///
@external(erlang, "simplifile_erl", "is_symlink")
@external(javascript, "./simplifile_js.mjs", "isSymlink")
pub fn is_symlink(filepath: String) -> Result(Bool, FileError)

/// Creates an empty file at the given filepath. Returns an `Error(Eexist)`
/// if the file already exists.
///
pub fn create_file(at filepath: String) -> Result(Nil, FileError) {
  case filepath |> is_file, filepath |> is_directory {
    Ok(True), _ | _, Ok(True) -> Error(Eexist)
    _, _ -> write_bits(<<>>, to: filepath)
  }
}

/// Recursively creates necessary directories for a given directory
/// path. Note that if you pass a path that "looks like" a file, i.e.
/// `./a/b.txt`, a folder named `b.txt` will be created, so be sure
/// to pass only the path to the required directory.
pub fn create_directory_all(dirpath: String) -> Result(Nil, FileError) {
  let is_abs = filepath.is_absolute(dirpath)
  let path =
    dirpath
    |> filepath.split
    |> list.fold("", filepath.join)
  let path = case is_abs {
    True -> "/" <> path
    False -> path
  }
  do_create_dir_all(path <> "/")
}

@external(erlang, "simplifile_erl", "create_dir_all")
@external(javascript, "./simplifile_js.mjs", "createDirAll")
fn do_create_dir_all(dirpath: String) -> Result(Nil, FileError)

/// Copy a file or a directory to a new path. Copies directories recursively.
/// 
/// ### Performance Note 
/// This function does work to determine if the src path
/// points to a file or a directory. Consider using one of the the dedicated 
/// functions `copy_file` or `copy_directory` if you already know which one you need.
pub fn copy(src src: String, dest dest: String) -> Result(Nil, FileError) {
  use src_info <- result.try(file_info(src))
  case file_info_type(src_info) {
    File -> copy_file(src, dest)
    Directory -> copy_directory(src, dest)
    Symlink ->
      Error(Unknown(
        "This is an internal bug where the `file_info` is somehow returning info about a simlink. Please file an issue on the simplifile repo.",
      ))
    Other ->
      Error(Unknown("Unknown file type (not file, directory, or simlink)"))
  }
}

/// Copy a file at a given path to another path.
/// Note: destination should include the filename, not just the directory
pub fn copy_file(at src: String, to dest: String) -> Result(Nil, FileError) {
  do_copy_file(src, dest)
  |> result.replace(Nil)
}

@external(erlang, "file", "copy")
@external(javascript, "./simplifile_js.mjs", "copyFile")
fn do_copy_file(src: String, dest: String) -> Result(Int, FileError)

/// Rename a file at a given path to another path.
/// Note: destination should include the filename, not just the directory
@deprecated("This function can move a file or a directory, so it's being renamed `rename`.")
@external(erlang, "simplifile_erl", "rename_file")
@external(javascript, "./simplifile_js.mjs", "renameFile")
pub fn rename_file(at src: String, to dest: String) -> Result(Nil, FileError)

/// Rename a file or directory.
@external(erlang, "simplifile_erl", "rename_file")
@external(javascript, "./simplifile_js.mjs", "renameFile")
pub fn rename(at src: String, to dest: String) -> Result(Nil, FileError)

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
    let src_path = filepath.join(src, segment)
    let dest_path = filepath.join(dest, segment)

    use src_info <- result.try(file_info(src_path))
    case file_info_type(src_info) {
      File -> {
        // For a file, create the file in the new directory
        use content <- result.try(read_bits(src_path))
        content
        |> write_bits(to: dest_path)
      }
      Directory -> {
        // Create the target directory and recurse
        use _ <- result.try(create_directory(dest_path))
        do_copy_directory(src_path, dest_path)
      }
      // Theoretically this shouldn't happen, as the file info function
      // will follow a simlink.
      Symlink ->
        Error(Unknown(
          "This is an internal bug where the `file_info` is somehow returning info about a simlink. Please file an issue on the simplifile repo.",
        ))
      Other ->
        Error(Unknown("Unknown file type (not file, directory, or simlink)"))
    }
  })
  Ok(Nil)
}

/// Copy a directory recursively and then delete the old one.
@deprecated("Use the `rename` function, which can rename a file or a directory.")
pub fn rename_directory(
  at src: String,
  to dest: String,
) -> Result(Nil, FileError) {
  use _ <- result.try(copy_directory(src, dest))
  delete(src)
}

/// Clear the contents of a directory, deleting all files and directories within
/// but leaving the top level directory in place.
pub fn clear_directory(at path: String) -> Result(Nil, FileError) {
  use paths <- result.try(read_directory(path))
  paths
  |> list.map(filepath.join(path, _))
  |> delete_all
}

/// Returns a list of filepaths for every file in the directory, including nested
/// files.
///
pub fn get_files(in directory: String) -> Result(List(String), FileError) {
  use contents <- result.try(read_directory(directory))
  use acc, content <- list.try_fold(over: contents, from: [])
  let path = filepath.join(directory, content)
  use info <- result.try(file_info(path))

  case file_info_type(info) {
    File -> Ok([path, ..acc])
    Directory -> {
      use nested_files <- result.try(get_files(path))
      Ok(list.append(acc, nested_files))
    }
    _ -> Ok(acc)
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

fn integer_to_permissions(integer: Int) -> Set(Permission) {
  case int.bitwise_and(integer, 7) {
    7 -> set.from_list([Read, Write, Execute])
    6 -> set.from_list([Read, Write])
    5 -> set.from_list([Read, Execute])
    3 -> set.from_list([Write, Execute])
    4 -> set.from_list([Read])
    2 -> set.from_list([Write])
    1 -> set.from_list([Execute])
    0 -> set.new()
    // since  we bitwise_and, these are all possible values.
    _ -> panic
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

fn octal_to_file_permissions(octal: Int) -> FilePermissions {
  FilePermissions(
    user: octal
      |> int.bitwise_shift_right(6)
      |> integer_to_permissions,
    group: octal
      |> int.bitwise_shift_right(3)
      |> integer_to_permissions,
    other: octal
      |> integer_to_permissions,
  )
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
@external(erlang, "simplifile_erl", "set_permissions_octal")
@external(javascript, "./simplifile_js.mjs", "setPermissionsOctal")
pub fn set_permissions_octal(
  for_file_at filepath: String,
  to permissions: Int,
) -> Result(Nil, FileError)

/// Returns the current working directory
///
@external(javascript, "./simplifile_js.mjs", "currentDirectory")
pub fn current_directory() -> Result(String, FileError) {
  erl_do_current_directory()
  |> result.map(string.from_utf_codepoints)
}

@external(erlang, "file", "get_cwd")
fn erl_do_current_directory() -> Result(List(UtfCodepoint), FileError)
