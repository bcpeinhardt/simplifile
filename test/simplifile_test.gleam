import gleeunit
import gleeunit/should

import simplifile.{read, write, delete, append, read_bits, write_bits, append_bits, Enoent}

pub fn main() {
  gleeunit.main()
}

pub fn main_test() {
  let filepath = "./test/hello.txt"
  let assert Ok(_) = "Hello, World" |> write(to: filepath)
  let assert Ok(_) = "Goodbye, Mars" |> append(to: filepath)
  let assert Ok("Hello, WorldGoodbye, Mars") = read(from: filepath)
  let assert Ok(_) = delete(filepath)
  let assert Error(_) = read(from: filepath)
}

pub fn bits_test() {
  let filepath = "./test/hello_bits.txt"
  let assert Ok(_) = <<"Hello, World":utf8>> |> write_bits(to: filepath)
  let assert Ok(_) = <<"Goodbye, Mars":utf8>> |> append_bits(to: filepath)
  let assert Ok(hello_goodbye) = read_bits(from: filepath)
  hello_goodbye |> should.equal(<<"Hello, WorldGoodbye, Mars":utf8>>)
  let assert Ok(_) = delete(filepath)
  let assert Error(_) = read_bits(from: filepath)
}

pub fn reason_test() {

  // ENOENT
  let filepath = "./test/hello_bits.txt"
  let assert Error(e) = read(from: filepath)
  e |> should.equal(Enoent)

  let assert Error(e) = delete(file_at: filepath)
  e |> should.equal(Enoent)
}
