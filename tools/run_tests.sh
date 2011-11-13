#! /bin/sh

err_count=0
target=native
failed_tests=

fail()
{
    echo "$1 $src"
    err_count=`expr 1 + $err_count`
    failed_tests="$failed_tests $src"
}

trap 'exit 130' INT

for src in "$@"; do
    p="${src%.*}.$target"
    echo ${OCAMLBUILD-ocamlbuild} -no-links -quiet $p
    ${OCAMLBUILD-ocamlbuild} -no-links -quiet $p || { fail FC $src; continue; }
    _build/$p || { fail FR $src; continue; }
    echo "OK $p"
done

if [ $err_count -ne 0 ]; then
    echo "$err_count of $# tests failed:$failed_tests"
    exit 69
else
    echo "All $# tests succeeded."
fi
