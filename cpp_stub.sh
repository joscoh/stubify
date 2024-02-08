#!/bin/bash

BIN=_build/default/bin/main.exe

#C file to preprocess
ARG=$1
FUNCTIONS=$2

#invoke the modified preprocessor, output to stdout
gcc -m64 -U__GNUC__ -U__SIZEOF_INT128__ -E -std=c99 -D__COMPCERT__ -D__COMPCERT_MAJOR__=3 \
-D__COMPCERT_MINOR__=14 -D__COMPCERT_VERSION__=314 -U__STDC_IEC_559_COMPLEX__ \
-D__STDC_NO_ATOMICS__ -D__STDC_NO_COMPLEX__ -D__STDC_NO_THREADS__ -D__STDC_NO_VLA__ \
-D__COMPCERT_WCHAR_TYPE__=int -I/usr/local/lib/compcert/include \
"$ARG" | \
$BIN -std c11 -names $FUNCTIONS