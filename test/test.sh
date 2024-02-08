#!/bin/bash

cd ..

./cpp_stub.sh test/a.c test/names.txt > test/a_stub.i
./cpp_stub.sh test/b.c test/names.txt > test/b_stub.i

cd test

ccomp a_stub.i b_stub.i main.c stub_error.c -o main.o