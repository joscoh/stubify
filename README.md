# Stubify - A Tool to Generate Minimial Dependencies

This is a slightly modified C parser which replaces user-defined functions with a trivial stub (preserving line numbers and preprocessing directives) which throws an error if the function is called.
It is based on the open-source CompCert C parser.

This relies on OCaml 4.13.1 and Dune 3.7.0.
It does not rely directly on CompCert, but the resulting preprocessed C files will almost certainly need to be compiled with CompCert. 

## Building and Running

The build the parser/stubify tool, run `dune build` from the root directory.
One way to run the tool is to run from the root directory:
`./cpp_stub.sh foo.c names.txt > foo_stub.i`
where `foo.c` is the input C file, `names.txt` contains the names (one per line) of the C functions to keep, and `foo_stub.i` is the output file.

A test is avaiable in the `test/` directory.
Running `test/test.sh` generates stub versions of `test/a.c` and `test/b.c` and uses CompCert to compile the C program `main.c` and the stubbed versions to
produce an executable `test/main.o`.