import gleeunit
import gleeunit/should
import simplifile.{
  Eacces, Enoent, Execute, FilePermissions, NotUtf8, Read, Write, append,
  append_bits, copy_directory, copy_file, create_directory, create_directory_all,
  create_file, delete, delete_all, file_info, file_permissions_to_octal,
  get_files, read, read_bits, read_directory, rename_directory, rename_file,
  set_permissions, set_permissions_octal, verify_is_directory, verify_is_file,
  write, write_bits,
}
import gleam/list
import gleam/int
import gleam/set

// import gleam/io

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
  let assert Ok(_) =
    <<"Goodbye, Mars":utf8>>
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

  let assert Ok(False) = verify_is_directory(filepath)

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

pub fn read_directory_test() {
  // Test setup
  let test_dir = "./tmp/test_dir"
  let assert Ok(_) = create_directory(test_dir)
  let assert Ok(True) = verify_is_directory(test_dir)
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
  let assert Ok(False) = verify_is_file(filepath)
  let assert Ok(_) =
    ""
    |> write(to: filepath)
  let assert Ok(True) = verify_is_file(filepath)
  let assert Ok(_) = delete(file_or_dir_at: filepath)
  let assert Ok(False) = verify_is_file(filepath)

  // A directory is not a file
  let assert Ok(False) = verify_is_file("./tmp")
}

pub fn is_directory_test() {
  let assert Ok(True) = verify_is_directory("./tmp")
  let assert Ok(False) = verify_is_directory("./does_not_exist")

  // A file is not a directory
  let assert Ok(False) = verify_is_directory("./simplifile.gleam")
}

pub fn create_all_test() {
  let assert Ok(_) = create_directory_all("./tmp/level1/level2")
  let assert Ok(True) = verify_is_directory("./tmp/level1")
  let assert Ok(True) = verify_is_directory("./tmp/level1/level2")
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
    rename_file("./tmp/to_be_renamed.txt", to: "./tmp/renamed.txt")
  let assert Ok(False) = verify_is_file("./tmp/to_be_renamed.txt")
  let assert Ok(True) = verify_is_file("./tmp/renamed.txt")
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
  let assert Ok(_) =
    rename_directory("./tmp/to_be_copied_dir", to: "./tmp/copied_dir")

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

pub fn get_files_with_slash_test() {
  let assert Ok(files) = get_files(in: "./test/")
  files
  |> should.equal(["./test/simplifile_test.gleam"])
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

  let assert Ok(True) = verify_is_file(existing_file)
  let assert Ok(True) = verify_is_directory(existing_dir)

  let assert Ok(False) = verify_is_directory(existing_file)
  let assert Ok(False) = verify_is_file(existing_dir)

  let assert Ok(False) = verify_is_file(non_existing_file)
  let assert Ok(False) = verify_is_directory(non_existing_file)
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
  let assert Error(Eacces) = verify_is_file(new_file)
  let assert Error(Eacces) = verify_is_directory(new_dir)

  // Cleanup
  let assert Ok(Nil) = set_permissions_octal(parent_dir, 0o777)
  let assert Ok(Nil) = delete(parent_dir)
}

pub fn file_info_test() {
  let assert Ok(_info) = file_info("./test.sh")
}
/// I visually inspected this info to make sure it matched on all targets.
/// TODO: Add a better test setup for validating file info functionality.
