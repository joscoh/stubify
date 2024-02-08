# Stubify - A Tool to Generate Minimial Dependencies

This is a slightly modified C parser which replaces user-defined functions with a trivial stub (preserving line numbers and preprocessing directives) which throws an error if the function is called.
It is based on the open-source CompCert C parser.
To build, run "dune build".
