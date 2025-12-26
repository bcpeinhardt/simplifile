import gleam/int
import gleam/list
import gleam/set
import gleam/string
import gleeunit
import gleeunit/should
import simplifile.{
  type FileError, Directory, Eacces, Eagain, Ebadf, Ebadmsg, Ebusy, Edeadlk,
  Edeadlock, Edquot, Eexist, Efault, Efbig, Eftype, Einval, Eio, Eisdir, Eloop,
  Emfile, Emlink, Emultihop, Enametoolong, Enfile, Enobufs, Enodev, Enoent,
  Enolck, Enolink, Enomem, Enospc, Enosr, Enostr, Enosys, Enotblk, Enotdir,
  Enotsup, Enxio, Eopnotsupp, Eoverflow, Eperm, Epipe, Erange, Erofs, Espipe,
  Esrch, Estale, Etxtbsy, Exdev, Execute, File, FilePermissions, NotUtf8, Read,
  Unknown, Write, append, append_bits, copy, copy_directory, copy_file,
  create_directory, create_directory_all, create_file, create_link,
  create_symlink, delete, delete_all, file_info, file_info_permissions,
  file_info_permissions_octal, file_info_type, file_permissions_to_octal,
  get_files, is_directory, is_file, is_symlink, link_info, read, read_bits,
  read_directory, rename, set_permissions, set_permissions_octal, write,
  write_bits,
}

pub fn main() {
  let assert Ok(_) = delete_all(["./tmp"])
  let assert Ok(_) = create_directory("./tmp")
  gleeunit.main()
}

pub fn main_test() {
  let filepath = "./tmp/hello.txt"
  let assert Ok(_) =
    "Hello, World"
    |> write(to: filepath)
  let assert Ok(_) =
    "Goodbye, Mars"
    |> append(to: filepath)
  let assert Ok("Hello, WorldGoodbye, Mars") = read(from: filepath)
  let assert Ok(_) = delete(filepath)
  let assert Error(_) = read(from: filepath)
}

pub fn bits_test() {
  let filepath = "./tmp/hello_bits.txt"
  let assert Ok(_) =
    <<"Hello, World":utf8>>
    |> write_bits(to: filepath)
  let assert Error(_) =
    <<1:size(7)>>
    |> write_bits(to: filepath)
  let assert <<_:3, unaligned_text:bits>> = <<0:3, "Goodbye, Mars":utf8>>
  let assert Ok(_) =
    unaligned_text
    |> append_bits(to: filepath)
  let assert Error(_) =
    <<1:size(7)>>
    |> append_bits(to: filepath)
  let assert Ok(hello_goodbye) = read_bits(from: filepath)
  hello_goodbye
  |> should.equal(<<"Hello, WorldGoodbye, Mars":utf8>>)
  let assert Ok(_) = delete(filepath)
  let assert Error(_) = read_bits(from: filepath)
}

pub fn enoent_test() {
  // ENOENT
  let filepath = "./tmp/does_not_exist.txt"
  let assert Error(e) = read(from: filepath)
  e
  |> should.equal(Enoent)
}

pub fn path_test() {
  let filepath = "./tmp/path_test.txt"
  let assert Ok(_) =
    "Hello"
    |> write(to: filepath)

  let assert Ok(False) = is_directory(filepath)

  let assert Ok(_) = delete(file_or_dir_at: "./tmp/path_test.txt")
}

pub fn make_directory_test() {
  let the_directory = "./tmp/some_created_dir"
  let assert Ok(_) = create_directory(the_directory)
  let assert Error(_) = create_directory(the_directory)
  let assert Error(_) = create_directory("./test/simplifile_test.gleam")
  let assert Ok([]) = read_directory(the_directory)
  let assert Ok(_) = delete(the_directory)
}

pub fn make_symlink_test() {
  let the_target = "target_of_created_symlink"
  let the_symlink = "./tmp/created_symlink"
  let assert Ok(_) = create_symlink(the_target, the_symlink)
  let assert Error(_) = create_symlink(the_target, the_symlink)
  // /!\ with Deno runtime:
  //   if the symlink has been created with a non existing target,
  //   the symlink cannot be deleted (??).
  //   So we first create the target,
  //   then we delete the symlink,
  //   and finally we delete the target
  let assert Ok(_) =
    ""
    |> write(to: "./tmp/" <> the_target)
  let assert Ok(_) = delete(the_symlink)
  let assert Ok(_) = delete("./tmp/" <> the_target)
}

pub fn make_link_test() {
  let the_target = "target_of_created_link"
  let the_link = "./tmp/created_link"

  // File does not exist yet
  let assert Error(_) = create_link(the_target, the_link)

  let assert Ok(_) =
    "some txt"
    |> write(to: the_target)

  let assert Ok(_) = create_link(the_target, the_link)
  // Link already exists
  let assert Error(_) = create_link(the_target, the_link)

  let assert Ok("some txt") = read(the_link)

  let assert Ok(_) = delete(the_target)
  let assert Ok(_) = delete(the_link)
}

pub fn read_directory_test() {
  // Test setup
  let test_dir = "./tmp/test_dir"
  let assert Ok(_) = create_directory(test_dir)
  let assert Ok(True) = is_directory(test_dir)
  let assert Ok(_) =
    "some txt"
    |> write(to: test_dir <> "/test.txt")
  let assert Ok(_) =
    "some txt"
    |> write(to: test_dir <> "/test2.txt")
  let assert Ok(_) = create_directory(test_dir <> "/test_inner_dir")

  // List the contents
  let assert Ok(stuff) = read_directory(at: test_dir)
  let assert True = list.contains(stuff, "test.txt")
  let assert True = list.contains(stuff, "test2.txt")
  let assert True = list.contains(stuff, "test_inner_dir")

  // Verify errors on invalid uses
  let assert Error(_) = read_directory(at: "./test/simplifile_test.gleam")
  let assert Error(_) = read_directory(at: "./tmp/i_dont_exist")

  // Cleanup
  let assert Ok(_) = delete(test_dir)
}

pub fn non_utf_test() {
  let filepath = "./tmp/not_utf8.txt"
  let assert Ok(_) =
    <<0xC0>>
    |> write_bits(to: filepath)
  let assert Error(NotUtf8) = read(from: filepath)
  let assert Ok(_) = delete(file_or_dir_at: filepath)
}

pub fn is_file_test() {
  // Basic usage
  let filepath = "./tmp/is_file_test.txt"
  let assert Ok(False) = is_file(filepath)
  let assert Ok(_) =
    ""
    |> write(to: filepath)
  let assert Ok(True) = is_file(filepath)
  let assert Ok(_) = delete(file_or_dir_at: filepath)
  let assert Ok(False) = is_file(filepath)

  // A directory is not a file
  let assert Ok(False) = is_file("./tmp")
}

pub fn is_directory_test() {
  let assert Ok(True) = is_directory("./tmp")
  let assert Ok(False) = is_directory("./does_not_exist")

  // A file is not a directory
  let assert Ok(False) = is_directory("./simplifile.gleam")
}

pub fn is_symlink_test() {
  let the_target = "target_of_created_symlink"
  let the_symlink = "./tmp/created_symlink"
  let existing_file_target_for_symlink = "existing_file_target_for_symlink"
  let symlink_to_existing_file = "./tmp/symlink_to_existing_file"
  let existing_dir_target_for_symlink = "existing_dir_target_for_symlink"
  let symlink_to_existing_dir = "./tmp/symlink_to_existing_dir"
  let filepath = "./tmp/is_file_test.txt"
  let assert Ok(_) =
    ""
    |> write(to: filepath)
  let assert Ok(_) = create_symlink(the_target, the_symlink)
  let assert Ok(True) = is_symlink(the_symlink)
  let assert Ok(False) = is_symlink("./does_not_exist")
  // A symlink is not a file if the target doesn't exist
  let assert Ok(False) = is_file(the_symlink)
  // A symlink is a file if the file target does exist
  let assert Ok(_) =
    ""
    |> write(to: "./tmp/" <> existing_file_target_for_symlink)
  let assert Ok(_) =
    create_symlink(existing_file_target_for_symlink, symlink_to_existing_file)
  let assert Ok(True) = is_symlink(symlink_to_existing_file)
  let assert Ok(True) = is_file(symlink_to_existing_file)
  let assert Ok(False) = is_directory(symlink_to_existing_file)
  // A symlink is not a directory if the target doesn't exist
  let assert Ok(False) = is_directory(the_symlink)
  // A symlink is a directory if the directory target does exist
  let assert Ok(_) =
    create_directory("./tmp/" <> existing_dir_target_for_symlink)
  let assert Ok(_) =
    create_symlink(existing_dir_target_for_symlink, symlink_to_existing_dir)
  let assert Ok(True) = is_symlink(symlink_to_existing_dir)
  let assert Ok(True) = is_directory(symlink_to_existing_dir)
  let assert Ok(False) = is_file(symlink_to_existing_dir)
  // A file is not a symlink
  let assert Ok(False) = is_symlink(filepath)
  // A directory is not a symlink
  let assert Ok(False) = is_symlink("./tmp/")
  // Clean everything
  let assert Ok(_) = delete(file_or_dir_at: filepath)
  let assert Ok(_) = delete(symlink_to_existing_file)
  let assert Ok(_) = delete("./tmp/" <> existing_file_target_for_symlink)
  let assert Ok(_) = delete(symlink_to_existing_dir)
  let assert Ok(_) = delete("./tmp/" <> existing_dir_target_for_symlink)
  let assert Ok(_) = delete(the_symlink)
}

pub fn create_all_test() {
  let assert Ok(_) = create_directory_all("./tmp/level1/level2")
  let assert Ok(True) = is_directory("./tmp/level1")
  let assert Ok(True) = is_directory("./tmp/level1/level2")
  let assert Ok(_) = delete("./tmp/level1")
}

pub fn copy_test() {
  let assert Ok(_) = write("Hello", to: "./tmp/to_be_copied.txt")
  let assert Ok(Nil) =
    copy_file(at: "./tmp/to_be_copied.txt", to: "./tmp/copied.txt")
  let assert Ok(_) = delete(file_or_dir_at: "./tmp/to_be_copied.txt")
  let assert Ok("Hello") = read("./tmp/copied.txt")
  let assert Ok(_) = delete("./tmp/copied.txt")
}

pub fn rename_test() {
  let assert Ok(_) = write("Hello", to: "./tmp/to_be_renamed.txt")
  let assert Ok(Nil) =
    rename("./tmp/to_be_renamed.txt", to: "./tmp/renamed.txt")
  let assert Ok(False) = is_file("./tmp/to_be_renamed.txt")
  let assert Ok(True) = is_file("./tmp/renamed.txt")
  let assert Ok(_) = delete("./tmp/renamed.txt")
}

pub fn copy_directory_test() {
  // Make directory for copying
  let assert Ok(_) = create_directory("./tmp/to_be_copied_dir")
  let assert Ok(_) = create_directory("./tmp/to_be_copied_dir/nested_dir")
  let assert Ok(_) =
    "Hello"
    |> write(to: "./tmp/to_be_copied_dir/file.txt")
  let assert Ok(_) =
    "Hello"
    |> write(to: "./tmp/to_be_copied_dir/nested_dir/file.txt")

  // Copy the directory
  let assert Ok(_) =
    copy_directory("./tmp/to_be_copied_dir", to: "./tmp/copied_dir")

  // Assert the contents are correct
  let assert Ok("Hello") = read("./tmp/copied_dir/file.txt")
  let assert Ok("Hello") = read("./tmp/copied_dir/nested_dir/file.txt")

  // Cleanup
  let assert Ok(_) = delete("./tmp/to_be_copied_dir")
  let assert Ok(_) = delete("./tmp/copied_dir")
}

pub fn rename_directory_test() {
  // Make directory for copying
  let assert Ok(_) = create_directory("./tmp/to_be_copied_dir")
  let assert Ok(_) = create_directory("./tmp/to_be_copied_dir/nested_dir")
  let assert Ok(_) =
    "Hello"
    |> write(to: "./tmp/to_be_copied_dir/file.txt")
  let assert Ok(_) =
    "Hello"
    |> write(to: "./tmp/to_be_copied_dir/nested_dir/file.txt")

  // Copy the directory
  let assert Ok(_) = rename("./tmp/to_be_copied_dir", to: "./tmp/copied_dir")

  // Assert the contents are correct
  let assert Ok("Hello") = read("./tmp/copied_dir/file.txt")
  let assert Ok("Hello") = read("./tmp/copied_dir/nested_dir/file.txt")

  // Cleanup
  let assert Error(_) = read("./tmp/to_be_copied_dir")
  let assert Ok(_) = delete("./tmp/copied_dir")
}

pub fn copy_directory_nested_needs_create_all_test() {
  // Make directory for copying
  let assert Ok(_) = create_directory("./tmp/to_be_copied_dir")
  let assert Ok(_) = create_directory("./tmp/to_be_copied_dir/nested_dir")
  let assert Ok(_) =
    "Hello"
    |> write(to: "./tmp/to_be_copied_dir/file.txt")
  let assert Ok(_) =
    "Hello"
    |> write(to: "./tmp/to_be_copied_dir/nested_dir/file.txt")

  // Copy the directory
  let assert Ok(_) =
    copy_directory("./tmp/to_be_copied_dir", to: "./tmp/nest/nest/copied_dir")

  // Assert the contents are correct
  let assert Ok("Hello") = read("./tmp/nest/nest/copied_dir/file.txt")
  let assert Ok("Hello") =
    read("./tmp/nest/nest/copied_dir/nested_dir/file.txt")

  // Cleanup
  let assert Ok(_) = delete("./tmp/to_be_copied_dir")
  let assert Ok(_) = delete("./tmp/nest")
}

pub fn delete_test() {
  // Basic delete
  let assert Ok(_) = write("Hello", to: "./tmp/existing_file.txt")
  let assert Ok(_) = delete("./tmp/existing_file.txt")
  let assert Error(Enoent) = read("./tmp/existing_file.txt")

  // Deleting a file that doesn't exist throws an error
  let assert Error(Enoent) = delete("./idontexist")

  // Delete a symlink doesn't delete the target
  let the_target = "target_of_created_symlink"
  let the_symlink = "./tmp/created_symlink"
  let assert Ok(_) =
    ""
    |> write(to: "./tmp/" <> the_target)
  let assert Ok(_) = create_symlink(the_target, the_symlink)
  let assert Ok(True) = is_file("./tmp/" <> the_target)
  let assert Ok(True) = is_symlink(the_symlink)
  let assert Ok(_) = delete(the_symlink)
  let assert Ok(False) = is_symlink(the_symlink)
  let assert Ok(True) = is_file("./tmp/" <> the_target)
  let assert Ok(_) = delete(file_or_dir_at: "./tmp/" <> the_target)
  let assert Ok(False) = is_file("./tmp/" <> the_target)
}

pub fn delete_all_test() {
  let files =
    list.map([1, 2, 3, 4, 5], fn(item) {
      "./tmp/tmp" <> int.to_string(item) <> ".txt"
    })
  list.each(files, fn(item) { write("Hello", to: item) })
  let assert Ok(_) = delete_all(["./idontexist", ..files])
}

pub fn all_the_ways_to_write_a_string_to_a_file_test() {
  let assert Ok(Nil) = write("./tmp/alltheways.txt", "Hello")
  let assert Ok("Hello") = read("./tmp/alltheways.txt")

  let assert Ok(Nil) = write("./tmp/alltheways.txt", contents: "Goodbye")
  let assert Ok("Goodbye") = read("./tmp/alltheways.txt")

  let assert Ok(Nil) = write("Hello", to: "./tmp/alltheways.txt")
  let assert Ok("Hello") = read("./tmp/alltheways.txt")

  let assert Ok(Nil) =
    "Goodbye"
    |> write(to: "./tmp/alltheways.txt")
  let assert Ok("Goodbye") = read("./tmp/alltheways.txt")

  let assert Ok(Nil) =
    "./tmp/alltheways.txt"
    |> write(contents: "Hello")
  let assert Ok("Hello") = read("./tmp/alltheways.txt")
}

pub fn files_test() {
  let assert Ok(Nil) = create_directory_all("./tmp/1/2/3/4/5")
  let assert Ok(Nil) =
    create_directory_all("./tmp/1/empty_dir/nested_empty_dir")
  let assert Ok(Nil) = create_file("./tmp/1/test.txt")
  let assert Ok(Nil) = create_file("./tmp/1/2/test.txt")
  let assert Ok(Nil) = create_file("./tmp/1/2/3/test2.txt")
  let assert Ok(Nil) = create_file("./tmp/1/2/3/test.txt")
  let assert Ok(Nil) = create_file("./tmp/1/2/3/4/test.txt")
  let assert Ok(Nil) = create_file("./tmp/1/2/3/4/5/test.txt")

  let assert Ok(files) = get_files(in: "./tmp/1")
  list.length(files)
  |> should.equal(6)
  files
  |> set.from_list
  |> should.equal(
    set.from_list([
      "./tmp/1/test.txt", "./tmp/1/2/test.txt", "./tmp/1/2/3/test2.txt",
      "./tmp/1/2/3/test.txt", "./tmp/1/2/3/4/test.txt",
      "./tmp/1/2/3/4/5/test.txt",
    ]),
  )
}

pub fn octal_math_test() {
  let all = set.from_list([Read, Write, Execute])
  let all = FilePermissions(user: all, group: all, other: all)
  all
  |> file_permissions_to_octal
  |> should.equal(0o00777)
}

pub fn permissions_test() {
  let assert Ok(Nil) = create_directory("./tmp/permissions")
  let assert Ok(Nil) =
    write("echo \"Hello from a file\"", to: "./tmp/permissions/test.sh")

  // This is the equivalent of `chmod 777 ./tmp/permissions/test.sh`
  let all = set.from_list([Read, Write, Execute])
  let all = FilePermissions(user: all, group: all, other: all)
  let assert Ok(Nil) = set_permissions("./tmp/permissions/test.sh", all)
  let assert Ok(Nil) = delete("./tmp/permissions/test.sh")

  let assert Ok(Nil) =
    write("echo \"Hello from a file\"", to: "./tmp/permissions/test2.sh")
  let assert Ok(Nil) =
    set_permissions_octal("./tmp/permissions/test2.sh", 0o777)

  let assert Ok(Nil) = delete("./tmp/permissions/test2.sh")
  let assert Ok(Nil) = delete("./tmp/permissions")
}

pub fn permissions_octal_test() {
  let assert Ok(Nil) = create_directory("./tmp/permissions")
  let assert Ok(Nil) =
    write("echo \"Hello from a file\"", to: "./tmp/permissions/test2.sh")

  let assert Ok(Nil) =
    set_permissions_octal("./tmp/permissions/test2.sh", 0o777)
  // io.debug(res)
  let assert Ok(Nil) = delete("./tmp/permissions/test2.sh")
  let assert Ok(Nil) = delete("./tmp/permissions")
}

pub fn file_info_get_permissions_test() {
  let filepath = "./tmp/permissions/test.sh"
  let assert Ok(Nil) = create_directory("./tmp/permissions")
  let assert Ok(Nil) = write("echo \"Hello from a file\"", to: filepath)

  // This is the equivalent of `chmod 777 ./tmp/permissions/test.sh`
  let all = set.from_list([Read, Write, Execute])
  let all = FilePermissions(user: all, group: all, other: all)
  let assert Ok(Nil) = set_permissions(filepath, all)
  let assert Ok(info) = file_info(filepath)

  file_info_permissions(info)
  |> should.equal(all)

  file_info_permissions_octal(info)
  |> should.equal(0o777)

  let assert Ok(Nil) = set_permissions_octal(filepath, 0o625)
  let assert Ok(info) = file_info(filepath)

  file_info_permissions(info)
  |> should.equal(FilePermissions(
    user: set.from_list([Read, Write]),
    group: set.from_list([Write]),
    other: set.from_list([Read, Execute]),
  ))

  let assert Ok(Nil) = delete("./tmp/permissions")
}

pub fn get_files_with_slash_test() {
  let assert Ok(files) = get_files(in: "./test/")
  files
  |> should.equal([
    "./test/simplifile_test.gleam",
  ])
}

// This test is only for local development
// pub fn current_directory_test() {
//   Ok("/Users/benjaminpeinhardt/Development/Projects/simplifile")
//   |> should.equal(current_directory())
// }

pub fn verify_is_file_and_is_dir_test() {
  let existing_file = "./gleam.toml"
  let existing_dir = "./test"
  let non_existing_file = "./i_dont_exist"

  let assert Ok(True) = is_file(existing_file)
  let assert Ok(True) = is_directory(existing_dir)

  let assert Ok(False) = is_directory(existing_file)
  let assert Ok(False) = is_file(existing_dir)

  let assert Ok(False) = is_file(non_existing_file)
  let assert Ok(False) = is_directory(non_existing_file)
}

pub fn no_read_permissions_test() {
  // Create a directory with a test file and a test directory inside, then
  // remove all its permissions
  let parent_dir = "./tmp/no_read_permissions_dir"
  let new_file = "./tmp/no_read_permissions_dir/no_read_permissions.txt"
  let new_dir = "./tmp/no_read_permissions_dir/no_read_permissions"
  let assert Ok(Nil) = create_directory(parent_dir)
  let assert Ok(Nil) = create_directory(new_dir)
  let assert Ok(Nil) = write("Hello", to: new_file)
  let assert Ok(Nil) = set_permissions_octal(parent_dir, 0o000)

  // Verify that we can't read the file or directory
  let assert Error(Eacces) = is_file(new_file)
  let assert Error(Eacces) = is_directory(new_dir)

  // Cleanup
  let assert Ok(Nil) = set_permissions_octal(parent_dir, 0o777)
  let assert Ok(Nil) = delete(parent_dir)
}

pub fn file_info_test() {
  let assert Ok(_info) = file_info("./test.sh")
}

pub fn file_info_type_test() {
  let filepath = "./tmp/file_info_type_test.txt"
  let assert Ok(_) = write(to: filepath, contents: "")
  let assert Ok(info) = file_info(filepath)
  file_info_type(info)
  |> should.equal(File)

  let assert Ok(info) = file_info("./tmp")
  file_info_type(info)
  |> should.equal(Directory)

  let linkpath = "./tmp/file_info_type_symlink"
  let assert Ok(_) = create_symlink("file_info_type_test.txt", linkpath)
  let assert Ok(info) = file_info(linkpath)
  file_info_type(info)
  |> should.equal(File)
  // let assert Ok(info) = link_info(linkpath)
  // file_info_type(info)
  // |> should.equal(Symlink)
}

pub fn link_info_test() {
  let target_path = "./tmp/the_target"
  let symlink_path = "./tmp/the_symlink"
  let target_relative_to_symlink = "the_target"

  let assert Ok(_) = write(to: target_path, contents: "Wibble")
  let assert Ok(_) = create_symlink(target_relative_to_symlink, symlink_path)

  let assert Ok(lstat) = link_info(symlink_path)
  let assert Ok(stat) = file_info(symlink_path)

  stat
  |> should.not_equal(lstat)

  stat.size
  |> should.equal(6)

  lstat.size
  |> should.not_equal(6)
}

pub fn clear_directory_test() {
  let assert Ok(_) = create_directory_all("./tmp/clear_dir")
  let assert Ok(_) = create_directory_all("./tmp/clear_dir/nested_dir")
  let assert Ok(_) = create_file("./tmp/clear_dir/test.txt")
  let assert Ok(_) = create_file("./tmp/clear_dir/nested_dir/test.txt")

  let assert Ok(_) = simplifile.clear_directory("./tmp/clear_dir")
  is_directory("./tmp/clear_dir")
  |> should.equal(Ok(True))
  let assert Ok([]) = read_directory("./tmp/clear_dir")
  let assert Ok(_) = delete("./tmp/clear_dir")
}

pub fn deno_symlink_error_test() {
  let assert Ok(_) = create_file("./tmp/target.txt")
  let assert Ok(_) =
    create_symlink(from: "./tmp/simulated", to: "./tmp/target.txt")
  let assert Ok(_) = delete("./tmp/simulated")
}

pub fn describe_error_test() {
  let assert "Operation not permitted" = simplifile.describe_error(Eperm)

  let assert "No such file or directory" = simplifile.describe_error(Enoent)

  let assert "No such process" = simplifile.describe_error(Esrch)

  let assert "Input/output error" = simplifile.describe_error(Eio)

  let assert "Device not configured" = simplifile.describe_error(Enxio)

  let assert "Bad file descriptor" = simplifile.describe_error(Ebadf)

  let assert "Resource deadlock avoided" = simplifile.describe_error(Edeadlk)

  let assert "Resource deadlock avoided" = simplifile.describe_error(Edeadlock)

  let assert "Cannot allocate memory" = simplifile.describe_error(Enomem)

  let assert "Permission denied" = simplifile.describe_error(Eacces)

  let assert "Bad address" = simplifile.describe_error(Efault)

  let assert "Block device required" = simplifile.describe_error(Enotblk)

  let assert "Resource busy" = simplifile.describe_error(Ebusy)

  let assert "File exists" = simplifile.describe_error(Eexist)

  let assert "Cross-device link" = simplifile.describe_error(Exdev)

  let assert "Operation not supported by device" =
    simplifile.describe_error(Enodev)

  let assert "Not a directory" = simplifile.describe_error(Enotdir)

  let assert "Is a directory" = simplifile.describe_error(Eisdir)

  let assert "Invalid argument" = simplifile.describe_error(Einval)

  let assert "Too many open files in system" = simplifile.describe_error(Enfile)

  let assert "Too many open files" = simplifile.describe_error(Emfile)

  let assert "Text file busy" = simplifile.describe_error(Etxtbsy)

  let assert "File too large" = simplifile.describe_error(Efbig)

  let assert "No space left on device" = simplifile.describe_error(Enospc)

  let assert "Illegal seek" = simplifile.describe_error(Espipe)

  let assert "Read-only file system" = simplifile.describe_error(Erofs)

  let assert "Too many links" = simplifile.describe_error(Emlink)

  let assert "Broken pipe" = simplifile.describe_error(Epipe)

  let assert "Result too large" = simplifile.describe_error(Erange)

  let assert "Resource temporarily unavailable" =
    simplifile.describe_error(Eagain)

  let assert "Operation not supported" = simplifile.describe_error(Enotsup)

  let assert "No buffer space available" = simplifile.describe_error(Enobufs)

  let assert "Too many levels of symbolic links" =
    simplifile.describe_error(Eloop)

  let assert "File name too long" = simplifile.describe_error(Enametoolong)

  let assert "Disc quota exceeded" = simplifile.describe_error(Edquot)

  let assert "Stale NFS file handle" = simplifile.describe_error(Estale)

  let assert "No locks available" = simplifile.describe_error(Enolck)

  let assert "Function not implemented" = simplifile.describe_error(Enosys)

  let assert "Inappropriate file type or format" =
    simplifile.describe_error(Eftype)

  let assert "Value too large to be stored in data type" =
    simplifile.describe_error(Eoverflow)

  let assert "Bad message" = simplifile.describe_error(Ebadmsg)

  let assert "Multihop attempted" = simplifile.describe_error(Emultihop)

  let assert "Link has been severed" = simplifile.describe_error(Enolink)

  let assert "No STREAM resources" = simplifile.describe_error(Enosr)

  let assert "Not a STREAM" = simplifile.describe_error(Enostr)

  let assert "Operation not supported on socket" =
    simplifile.describe_error(Eopnotsupp)

  let assert "File not UTF-8 encoded" = simplifile.describe_error(NotUtf8)

  let assert "Unknown error: Something went wrong" =
    simplifile.describe_error(Unknown("Something went wrong"))
}

pub fn file_info_follows_simlinks_recursively_test() {
  let assert Ok(_) = create_file("./tmp/base.txt")
  let assert Ok(_) = create_symlink(from: "./tmp/layer_1.txt", to: "./base.txt")
  let assert Ok(_) =
    create_symlink(from: "./tmp/layer_2.txt", to: "./layer_1.txt")

  let assert Ok(fi) = file_info("./tmp/layer_2.txt")
  fi |> file_info_type |> should.equal(File)

  let assert Ok(_) = create_directory("./tmp/base_dir")
  let assert Ok(_) = create_symlink(from: "./tmp/layer_1_dir", to: "./base_dir")
  let assert Ok(_) =
    create_symlink(from: "./tmp/layer_2_dir", to: "./layer_1_dir")

  let assert Ok(fi) = file_info("./tmp/layer_2_dir")
  fi |> file_info_type |> should.equal(Directory)
}

pub fn copy_can_copy_whatever_test() {
  let og_file = "./tmp/toodaloofile.txt"
  let og_dir = "./tmp/toodaloofile_dir"
  let symlink_to_file = "./tmp/toodaloolink.txt"

  let assert Ok(_) = write("Hello", to: og_file)
  let assert Ok(_) = create_directory(og_dir)
  let assert Ok(_) = write("Hello subfile", to: og_dir <> "/subfile.txt")
  let assert Ok(_) =
    create_symlink(from: symlink_to_file, to: "./toodaloofile.txt")

  let assert Ok(_) = copy(og_file, "./tmp/toodaloo_copied.txt")
  let assert Ok("Hello") = read("./tmp/toodaloo_copied.txt")

  let assert Ok(_) = copy(og_dir, "./tmp/toodaloo_copied_dir")
  let assert Ok(["subfile.txt"]) = read_directory("./tmp/toodaloo_copied_dir")
  let assert Ok("Hello subfile") = read("./tmp/toodaloo_copied_dir/subfile.txt")

  let assert Ok(_) = copy(symlink_to_file, "./tmp/copied_link.txt")
  let assert Ok("Hello") = read("./tmp/copied_link.txt")
}

pub fn rename_file_succeeds_at_renaming_a_directory_test() {
  // I am so dumb lol

  let dir = "./tmp/i_am_a_dir"
  let file = dir <> "/i_am_a_file.txt"
  let assert Ok(_) = create_directory_all(dir)
  let assert Ok(_) = write("Hello", to: file)
  let new_dir = "./tmp/i_am_also_a_dir"

  let assert Ok(_) = rename(at: dir, to: new_dir)

  let assert Ok(fi) = file_info(new_dir)
  fi |> file_info_type |> should.equal(Directory)
  read(new_dir <> "/i_am_a_file.txt") |> should.be_ok |> should.equal("Hello")
}

pub fn parse_errors_test() {
  // Ensuring the ENOTDIR comes through correctly
  simplifile.create_file("./tmp/wumbo") |> should.be_ok
  let err =
    simplifile.create_directory_all("./tmp/wumbo/wombo") |> should.be_error
  err |> should.equal(Enotdir)
}

pub fn unknown_errors_return_unknown_test() {
  let err = create_directory_with_bad_arg(#(Nil, Nil))
  err |> should.be_error

  let assert Error(unknown) = err
  let assert Unknown(inner) = unknown
  echo inner
  inner |> should.not_equal("")
  inner |> string.uppercase |> should.equal(inner)
  // confirm the string has been uppercased
}

// This is necessary to force unknown error generation uniformly across runtimes
@external(erlang, "simplifile_erl", "create_directory")
@external(javascript, "./simplifile_js.mjs", "createDirectory")
fn create_directory_with_bad_arg(arg: #(Nil, Nil)) -> Result(Nil, FileError)
