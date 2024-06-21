#!/bin/sh

set -xe

PROG=("main")

for p in "${PROG[@]}"; do
    sbcl --eval "(compile-file \"${p}.lisp\")" --eval '(quit)' > /dev/null 2>&1;
    chmod +x "${p}.fasl";
done
