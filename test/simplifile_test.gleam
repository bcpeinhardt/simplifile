import gleeunit
import gleeunit/should
import simplifile.{
  Enoent, append, append_bits, delete, delete_directory, is_directory,
  list_contents, make_directory, read, read_bits, write, write_bits,
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

pub fn reason_test() {
  // ENOENT
  let filepath = "./test/hello_bits.txt"
  let assert Error(e) = read(from: filepath)
  e
  |> should.equal(Enoent)

  let assert Error(e) = delete(file_at: filepath)
  e
  |> should.equal(Enoent)
}

pub fn path_test() {
  let filepath = "./test/testfile.txt"
  let assert Ok(_) =
    "Hello"
    |> write(to: filepath)

  let assert False = is_directory(filepath)

  let assert Ok(_) = delete(file_at: "./test/testfile.txt")
}

pub fn make_directory_test() {
  let the_directory = "./test/some_created_dir"
  let assert Ok(_) = make_directory(the_directory)
  let assert Ok(_) = delete_directory(the_directory)
}

pub fn list_contents_test() {
  let curr_dir = "./test"
  let assert True = is_directory(curr_dir)
  let assert Ok(stuff) = list_contents(of: curr_dir)
  let assert True = list.contains(stuff, "another_dir")
  let assert True = list.contains(stuff, "test_dir")
  let assert True = list.contains(stuff, "simplifile_test.gleam")

  let assert Error(_) = list_contents(of: "./test/simplifile_test.gleam")
  let assert Error(_) = list_contents(of: "./test/i_dont_exist")
}
