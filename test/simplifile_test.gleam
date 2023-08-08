import gleeunit
import gleeunit/should
import simplifile.{
  Enoent, NotUtf8, append, append_bits, create_dir_all, create_directory, delete,
  is_directory, is_file, list_contents, read, read_bits, write, write_bits,
}
import gleam/list

pub fn main() {
  gleeunit.main()
}

pub fn main_test() {
  let filepath = "./test/hello.txt"
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
  let filepath = "./test/hello_bits.txt"
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
  let filepath = "./test/does_not_exist.txt"
  let assert Error(e) = read(from: filepath)
  e
  |> should.equal(Enoent)
}

pub fn path_test() {
  let filepath = "./test/path_test.txt"
  let assert Ok(_) =
    "Hello"
    |> write(to: filepath)

  let assert False = is_directory(filepath)

  let assert Ok(_) = delete(file_or_dir_at: "./test/path_test.txt")
}

pub fn make_directory_test() {
  let the_directory = "./test/some_created_dir"
  let assert Ok(_) = create_directory(the_directory)
  let assert Error(_) = create_directory(the_directory)
  let assert Error(_) = create_directory("./test/simplifile_test.gleam")
  let assert Ok([]) = list_contents(the_directory)
  let assert Ok(_) = delete(the_directory)
}

pub fn list_contents_test() {
  // Test setup
  let test_dir = "./test/test_dir"
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
  let assert Ok(stuff) = list_contents(of: test_dir)
  let assert True = list.contains(stuff, "test.txt")
  let assert True = list.contains(stuff, "test2.txt")
  let assert True = list.contains(stuff, "test_inner_dir")

  // Verify errors on invalid uses
  let assert Error(_) = list_contents(of: "./test/simplifile_test.gleam")
  let assert Error(_) = list_contents(of: "./test/i_dont_exist")

  // Cleanup
  let assert Ok(_) = delete(test_dir)
}

pub fn non_utf_test() {
  let filepath = "./test/not_utf8.txt"
  let assert Ok(_) =
    <<0xC0>>
    |> write_bits(to: filepath)
  let assert Error(NotUtf8) = read(from: filepath)
  let assert Ok(_) = delete(file_or_dir_at: filepath)
}

pub fn is_file_test() {
  // Basic usage
  let filepath = "./test/is_file_test.txt"
  let assert False = is_file(filepath)
  let assert Ok(_) =
    ""
    |> write(to: filepath)
  let assert True = is_file(filepath)
  let assert Ok(_) = delete(file_or_dir_at: filepath)
  let assert False = is_file(filepath)

  // A directory is not a file
  let assert False = is_file("./test")
}

pub fn is_directory_test() {
  let assert True = is_directory("./test")
  let assert False = is_directory("./does_not_exist")

  // A file is not a directory
  let assert False = is_directory("./simplifile.gleam")
}

pub fn create_all_test() {
  let assert Ok(_) = create_dir_all("./test/level1/level2")
  let assert True = is_directory("./test/level1")
  let assert True = is_directory("./test/level1/level2")
  let assert Ok(_) = delete("./test/level1")
}
