## Dependencies

In order to build the project, we need a few opam packages.
Using `opam`, run:

```
opam install dune menhir ppx_deriving ocaml-lsp-server ocamlformat-rpc
```

On the Rust side, a recent installation of cargo and rustc is needed.

## Usage

Building the project (both OCaml and Rust):

```
    make
```

Running the tests (this uses all the .lua files in the `../lua/` directory as test files):

```
    make test
```

To run the OCaml interpreter on a specific file `prog.lua`, move to the `ocaml` directory and type:

```
   dune exec --display=quiet interp/run.exe -- prog.lua
```

Alternatively, if one wants to use the CPS variant of the OCaml
interpreter (supporting coroutines), the she can use the command:

```
   dune exec --display=quiet interp-cps/run.exe -- prog.lua
```

To run the Rust interpreter on a specific file `prog.lua`, move to the `rust` directory and type:

```
   cargo run --release -- prog.lua
```
One can remove the `--release` option in order to disable optimizations.

To show the syntax tree corresponding to a mini-Lua program `prog.lua` after parsing by the OCaml parser (the AST in Rust should be equivalent):

```
    dune exec --display=quiet showast/showast.exe -- prog.lua
```

The *lua reference interpreter* source is in the `lua` directory. It can be built by typing `make refinterp`, and run using:

```
    lua/mini_lua.sh prog.lua
```
