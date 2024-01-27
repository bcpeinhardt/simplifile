import simplifile.{type FileError}
import gleam/list

pub type FilePath {
  FilePath(String)
}

pub type SourcePath {
  SourcePath(String)
}

pub type DestinationPath {
  DestinationPath(String)
}

/// Read a files contents as a string
/// ## Example
/// ```gleam
/// let assert Ok(records) = read(from: FilePath("./users.csv"))
/// ```
///
pub fn read(from filepath: FilePath) -> Result(String, FileError) {
  let filepath = filepath_to_string(filepath)
  simplifile.read(filepath)
}

/// Write a string to a file at the given path
/// ## Example
/// ```gleam
/// let assert Ok(Nil) = write("Hello, World!", to: FilePath("./hello_world.txt"))
/// ```
///
pub fn write(
  to filepath: FilePath,
  contents contents: String,
) -> Result(Nil, FileError) {
  let filepath = filepath_to_string(filepath)
  simplifile.write(to: filepath, contents: contents)
}

/// Delete a file or directory at a given path. Performs a recursive
/// delete on a directory.
/// Throws an error if the path does not exist.
/// ## Example
/// ```gleam
/// let assert Ok(Nil) = delete(file_at: FilePath("./delete_me.txt"))
/// ```
///
pub fn delete(file_or_dir_at path: FilePath) -> Result(Nil, FileError) {
  let filepath = filepath_to_string(path)
  simplifile.delete(filepath)
}

/// Delete all files/directories specified in a list of paths.
/// Recursively deletes provided directories.
/// Does not return an error if one or more of the provided paths 
/// do not exist. 
/// 
pub fn delete_all(paths paths: List(FilePath)) -> Result(Nil, FileError) {
  let paths = list.map(paths, filepath_to_string)
  simplifile.delete_all(paths)
}

/// Append a string to the contents of a file at the given path
/// ## Example
/// ```gleam
/// let assert Ok(Nil) = append("more text", to: FilePath("./needs_more_text.txt"))
/// ```
///
pub fn append(
  to filepath: FilePath,
  contents contents: String,
) -> Result(Nil, FileError) {
  let filepath = filepath_to_string(filepath)
  simplifile.append(to: filepath, contents: contents)
}

/// Read a files contents as a bitstring
/// ## Example
/// ```gleam
/// let assert Ok(records) = read_bits(from: FilePath("./users.csv"))
/// ```
///
pub fn read_bits(from filepath: FilePath) -> Result(BitArray, FileError) {
  let filepath = filepath_to_string(filepath)
  simplifile.read_bits(filepath)
}

/// Write a bitstring to a file at the given path
/// ## Example
/// ```gleam
/// let assert Ok(Nil) = write_bits(<<"Hello, World!":utf8>>, to: FilePath("./hello_world.txt"))
/// ```
///
pub fn write_bits(
  to filepath: FilePath,
  bits bits: BitArray,
) -> Result(Nil, FileError) {
  let filepath = filepath_to_string(filepath)
  simplifile.write_bits(to: filepath, bits: bits)
}

/// Append a bitstring to the contents of a file at the given path
/// ## Example
/// ```gleam
/// let assert Ok(Nil) = append_bits(<<"more text":utf8>>, to: FilePath("./needs_more_text.txt"))
/// ```
///
pub fn append_bits(
  to filepath: FilePath,
  bits bits: BitArray,
) -> Result(Nil, FileError) {
  let filepath = filepath_to_string(filepath)
  simplifile.append_bits(to: filepath, bits: bits)
}

/// Checks if the provided filepath is a directory
/// ## Example
/// ```gleam
/// let assert True = is_directory(FilePath("./test"))
/// ```
/// 
@deprecated("Use `is_valid_directory` instead")
pub fn is_directory(filepath: FilePath) -> Bool {
  let filepath = filepath_to_string(filepath)
  simplifile.is_directory(filepath)
}

/// Checks if the provided filepath exists and is a directory.
/// Returns an error if it lacks permissions to read the directory.
/// 
/// ## Example
/// ```gleam
/// let assert Ok(True) = verify_is_directory(FilePath("./test"))
/// ```
pub fn verify_is_directory(filepath: FilePath) -> Result(Bool, FileError) {
  let filepath = filepath_to_string(filepath)
  simplifile.verify_is_directory(filepath)
}

/// Create a directory at the provided filepath. Returns an error if
/// the directory already exists.
///
/// ## Example
/// ```gleam
/// create_directory(FilePath("./test"))
/// ```
pub fn create_directory(filepath: FilePath) -> Result(Nil, FileError) {
  let filepath = filepath_to_string(filepath)
  simplifile.create_directory(filepath)
}

/// Lists the contents of a directory.
/// The list contains directory and file names, and is not recursive.
/// 
/// ## Example
/// ```gleam
/// let assert Ok(files_and_folders) = read_directory(at: FilePath("./Folder1"))
/// ```
/// 
pub fn read_directory(at path: FilePath) -> Result(List(String), FileError) {
  let filepath = filepath_to_string(path)
  simplifile.read_directory(filepath)
}

/// Returns `True` if there is a file at the given path, false otherwise.
/// 
@deprecated("Use `verify_is_file` instead")
pub fn is_file(filepath: FilePath) -> Bool {
  let filepath = filepath_to_string(filepath)
  simplifile.is_file(filepath)
}

/// Checks if the file at the provided filepath exists and is a file.
/// Returns an Error if it lacks permissions to read the file.
/// 
/// ## Example
/// ```gleam
/// let assert Ok(True) = verify_is_file(FilePath("./test.txt"))
/// ```
///
pub fn verify_is_file(filepath: FilePath) -> Result(Bool, FileError) {
  let filepath = filepath_to_string(filepath)
  simplifile.verify_is_file(filepath)
}

/// Creates an empty file at the given filepath. Returns an `Error(Eexist)`
/// if the file already exists.
/// 
pub fn create_file(at filepath: FilePath) -> Result(Nil, FileError) {
  let filepath = filepath_to_string(filepath)
  simplifile.create_file(filepath)
}

/// Recursively creates necessary directories for a given directory
/// path. Note that if you pass a path that "looks like" a file, i.e.
/// `./a/b.txt`, a folder named `b.txt` will be created, so be sure
/// to pass only the path to the required directory.
pub fn create_directory_all(dirpath: FilePath) -> Result(Nil, FileError) {
  let filepath = filepath_to_string(dirpath)
  simplifile.create_directory_all(filepath)
}

/// Copy a file at a given path to another path.
/// Note: destination should include the filename, not just the directory
pub fn copy_file(at src: SourcePath, to dest: DestinationPath) -> Result(Nil, FileError) {
  let SourcePath(source_path) = src
  let DestinationPath(destination_path) = dest
  simplifile.copy_file(at: source_path, to: destination_path)
}

/// Rename a file at a given path to another path.
/// Note: destination should include the filename, not just the directory
pub fn rename_file(at src: SourcePath, to dest: DestinationPath) -> Result(Nil, FileError) {
  let SourcePath(source_path) = src
  let DestinationPath(destination_path) = dest
  simplifile.rename_file(at: source_path, to: destination_path)
}

/// Copy a directory recursively
pub fn copy_directory(
  at src: SourcePath,
  to dest: DestinationPath,
) -> Result(Nil, FileError) {
  let SourcePath(source_path) = src
  let DestinationPath(destination_path) = dest
  simplifile.copy_directory(at: source_path, to: destination_path)
}

/// Copy a directory recursively and then delete the old one.
pub fn rename_directory(
  at src: SourcePath,
  to dest: DestinationPath,
) -> Result(Nil, FileError) {
  let SourcePath(source_path) = src
  let DestinationPath(destination_path) = dest
  simplifile.rename_directory(at: source_path, to: destination_path)
}

/// Returns a list of filepaths for every file in the directory, including nested
/// files.
/// 
pub fn get_files(in directory: FilePath) -> Result(List(String), FileError) {
  let filepath = filepath_to_string(directory)
  simplifile.get_files(filepath)
}

pub type Octal {
  Octal(Int)
}

pub fn file_permissions_to_octal(permissions: simplifile.FilePermissions) -> Int {
  simplifile.file_permissions_to_octal(permissions)
}

/// Sets the permissions for a given file
/// 
/// # Example
/// ```gleam
/// let all = set.from_list([Read, Write, Execute])
/// let all = FilePermissions(user: all, group: all, other: all)
/// let assert Ok(Nil) = set_permissions(FilePath("./script.sh"), all)
/// ```
pub fn set_permissions(
  for_file_at filepath: FilePath,
  to permissions: simplifile.FilePermissions,
) -> Result(Nil, FileError) {
  let filepath = filepath_to_string(filepath)
  simplifile.set_permissions(for_file_at: filepath, to: permissions)
}

/// Sets the permissions for a given file using an octal representation
/// 
/// # Example
/// ```gleam
/// set_permissions_octal(FilePath("./script.sh"), Octal(0o777))
/// ```
pub fn set_permissions_octal(
  for_file_at filepath: FilePath,
  to permissions: Octal,
) -> Result(Nil, FileError) {
  let Octal(octal) = permissions
  let filepath = filepath_to_string(filepath)
  simplifile.set_permissions_octal(for_file_at: filepath, to: octal)
}

/// Returns the current working directory
/// 
pub fn current_directory() -> Result(String, FileError) {
  simplifile.current_directory()
}

fn filepath_to_string(filepath: FilePath) {
  let FilePath(string) = filepath
  string
}
