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

if javascript {
  import gleam/result

  external fn read_(from: String) -> Result(String, String) =
    "./file.mjs" "readFile"

  /// Read a files contents as a string
  /// ## Example
  /// ```gleam
  /// let assert Ok(records) = read(from: "./users.csv")
  /// ```
  ///
  pub fn read(from from: String) -> Result(String, FileError) {
    read_(from)
    |> cast_error
  }

  external fn write_(String, to: String) -> Result(Nil, String) =
    "./file.mjs" "writeFile"

  /// Write a string to a given file
  /// ## Example
  /// ```gleam
  /// let assert Ok(Nil) = write("Hello, World!", to: "./hello_world.txt")
  /// ```
  ///
  pub fn write(contents: String, to filepath: String) -> Result(Nil, FileError) {
    write_(contents, to: filepath)
    |> cast_error
  }

  external fn delete_(file_at: String) -> Result(Nil, String) =
    "./file.mjs" "deleteFile"

  pub fn delete(file_at filepath: String) -> Result(Nil, FileError) {
    delete_(file_at: filepath)
    |> cast_error
  }

  external fn append_(String, to: String) -> Result(Nil, String) =
    "./file.mjs" "appendFile"

  pub fn append(contents: String, to filepath: String) -> Result(Nil, FileError) {
    append_(contents, to: filepath)
    |> cast_error
  }

  external fn read_bits_(from: String) -> Result(BitString, String) =
    "./file.mjs" "readBits"

  pub fn read_bits(from filepath: String) -> Result(BitString, FileError) {
    read_bits_(from: filepath)
    |> cast_error
  }

  external fn write_bits_(BitString, to: String) -> Result(Nil, String) =
    "./file.mjs" "writeBits"

  pub fn write_bits(
    contents: BitString,
    to filepath: String,
  ) -> Result(Nil, FileError) {
    write_bits_(contents, to: filepath)
    |> cast_error
  }

  external fn append_bits_(BitString, to: String) -> Result(Nil, String) =
    "./file.mjs" "appendBits"

  pub fn append_bits(
    contents: BitString,
    to filepath: String,
  ) -> Result(Nil, FileError) {
    append_bits_(contents, to: filepath)
    |> cast_error
  }

  fn cast_error(input: Result(a, String)) -> Result(a, FileError) {
    result.map_error(
      input,
      fn(e) {
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
      },
    )
  }
}

if erlang {
  import gleam/erlang/file.{Reason}
  import gleam/result

  /// Read a files contents as a string
  /// ## Example
  /// ```gleam
  /// let assert Ok(records) = read(from: "./users.csv")
  /// ```
  ///
  pub fn read(from filepath: String) -> Result(String, FileError) {
    file.read(filepath)
    |> cast_error
  }

  /// Write a string to a file at the given path
  /// ## Example
  /// ```gleam
  /// let assert Ok(Nil) = write("Hello, World!", to: "./hello_world.txt")
  /// ```
  ///
  pub fn write(contents: String, to filepath: String) -> Result(Nil, FileError) {
    file.write(contents, to: filepath)
    |> cast_error
  }

  /// Delete a file at a given filepath
  /// ## Example
  /// ```gleam
  /// let assert Ok(Nil) = delete(file_at: "./delete_me.txt")
  /// ```
  ///
  pub fn delete(file_at filepath: String) -> Result(Nil, FileError) {
    file.delete(filepath)
    |> cast_error
  }

  /// Append a string to the contents of a file at the given path
  /// ## Example
  /// ```gleam
  /// let assert Ok(Nil) = append("more text", to: "./needs_more_text.txt")
  /// ```
  ///
  pub fn append(contents: String, to filepath: String) -> Result(Nil, FileError) {
    file.append(contents, to: filepath)
    |> cast_error
  }

  /// Read a files contents as a bitstring
  /// ## Example
  /// ```gleam
  /// let assert Ok(records) = read_bits(from: "./users.csv")
  /// ```
  ///
  pub fn read_bits(from filepath: String) -> Result(BitString, FileError) {
    file.read_bits(filepath)
    |> cast_error
  }

  /// Write a bitstring to a file at the given path
  /// ## Example
  /// ```gleam
  /// let assert Ok(Nil) = write_bits(<<"Hello, World!":utf8>>, to: "./hello_world.txt")
  /// ```
  ///
  pub fn write_bits(
    bits: BitString,
    to filepath: String,
  ) -> Result(Nil, FileError) {
    file.write_bits(bits, filepath)
    |> cast_error
  }

  /// Append a string to the contents of a file at the given path
  /// ## Example
  /// ```gleam
  /// let assert Ok(Nil) = append_bits(<<"more text":utf8>>, to: "./needs_more_text.txt")
  /// ```
  ///
  pub fn append_bits(
    bits: BitString,
    to filepath: String,
  ) -> Result(Nil, FileError) {
    file.append_bits(bits, filepath)
    |> cast_error
  }

  fn cast_error(input: Result(a, Reason)) -> Result(a, FileError) {
    result.map_error(
      input,
      fn(e) {
        case e {
          file.Eacces -> Eacces
          file.Eagain -> Eagain
          file.Ebadf -> Ebadf
          file.Ebadmsg -> Ebadmsg
          file.Ebusy -> Ebusy
          file.Edeadlk -> Edeadlk
          file.Edeadlock -> Edeadlock
          file.Edquot -> Edquot
          file.Eexist -> Eexist
          file.Efault -> Efault
          file.Efbig -> Efbig
          file.Eftype -> Eftype
          file.Eintr -> Eintr
          file.Einval -> Einval
          file.Eio -> Eio
          file.Eisdir -> Eisdir
          file.Eloop -> Eloop
          file.Emfile -> Emfile
          file.Emlink -> Emlink
          file.Emultihop -> Emultihop
          file.Enametoolong -> Enametoolong
          file.Enfile -> Enfile
          file.Enobufs -> Enobufs
          file.Enodev -> Enodev
          file.Enolck -> Enolck
          file.Enolink -> Enolink
          file.Enoent -> Enoent
          file.Enomem -> Enomem
          file.Enospc -> Enospc
          file.Enosr -> Enosr
          file.Enostr -> Enostr
          file.Enosys -> Enosys
          file.Enotblk -> Enotblk
          file.Enotdir -> Enotdir
          file.Enotsup -> Enotsup
          file.Enxio -> Enxio
          file.Eopnotsupp -> Eopnotsupp
          file.Eoverflow -> Eoverflow
          file.Eperm -> Eperm
          file.Epipe -> Epipe
          file.Erange -> Erange
          file.Erofs -> Erofs
          file.Espipe -> Espipe
          file.Esrch -> Esrch
          file.Estale -> Estale
          file.Etxtbsy -> Etxtbsy
          file.Exdev -> Exdev
          file.NotUtf8 -> NotUtf8
        }
      },
    )
  }
}
