import gleeunit
import gleeunit/should
import simplifile.{
  Enoent, NotUtf8, append, append_bits, copy_directory, copy_file,
  create_directory, create_directory_all, delete, delete_all, is_directory,
  is_file, read, read_bits, read_directory, rename_directory, rename_file, write,
  write_bits,
}
import gleam/list
import gleam/int

pub fn main() {
  let assert Ok(_) = delete_all(["tmp"])
  let assert Ok(_) = create_directory("tmp")
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

  let assert False = is_directory(filepath)

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
  let assert True = is_directory(test_dir)
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
  let assert False = is_file(filepath)
  let assert Ok(_) =
    ""
    |> write(to: filepath)
  let assert True = is_file(filepath)
  let assert Ok(_) = delete(file_or_dir_at: filepath)
  let assert False = is_file(filepath)

  // A directory is not a file
  let assert False = is_file("./tmp")
}

pub fn is_directory_test() {
  let assert True = is_directory("./tmp")
  let assert False = is_directory("./does_not_exist")

  // A file is not a directory
  let assert False = is_directory("./simplifile.gleam")
}

pub fn create_all_test() {
  let assert Ok(_) = create_directory_all("./tmp/level1/level2")
  let assert True = is_directory("./tmp/level1")
  let assert True = is_directory("./tmp/level1/level2")
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
  let assert False = is_file("./tmp/to_be_renamed.txt")
  let assert True = is_file("./tmp/renamed.txt")
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

  // Deleting a file that doesn't exist throws an error
  let assert Error(Enoent) = delete("./idontexist")
}

pub fn delete_all_test() {
  let files =
    list.map(
      [1, 2, 3, 4, 5],
      fn(item) { "./tmp/tmp" <> int.to_string(item) <> ".txt" },
    )
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
