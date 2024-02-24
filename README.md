# simplifile

[![Package Version](https://img.shields.io/hexpm/v/simplifile)](https://hex.pm/packages/simplifile)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/simplifile/)

Simplifile provides synchronous operations for working with files and directories that work for all targets (Erlang, Node, and Deno). 

## When should I use `simplifile`?

Simplifile makes a simple tradeoff: it confines itself to a subset of purely synchronous file/directory utilities 
so that it can have one API for all targets. Erlang and JavaScript have fundamentally different concurrency
models, so bindings to async code will simply be different for different targets.

You *should* use simplifile if 
1. The same code running on JS and Erlang targets is important
2. Sync file operations will not be a major performance bottleneck (dev tooling, configuration, scripting, etc.)
3. You just wanna do some basic file operations and get on with life

If you think you need a different solutions, these projects may be helpful:
[File streams (erlang target)](https://github.com/richard-viney/file_streams)

## Example
```gleam
let filepath = "./test/hello.txt"
let assert Ok(_) = "Hello, World" |> write(to: filepath)
let assert Ok(_) = "Goodbye, Mars" |> append(to: filepath)
let assert Ok("Hello, WorldGoodbye, Mars") = read(from: filepath)
let assert Ok(_) = delete(filepath)
let assert Error(_) = read(from: filepath)
```

## Installation

If available on Hex this package can be added to your Gleam project:

```sh
gleam add simplifile
```

and its documentation can be found at <https://hexdocs.pm/simplifile>.
