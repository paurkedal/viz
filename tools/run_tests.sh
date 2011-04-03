#! /bin/sh

err_count=0
fail()
{
    echo "$1 $src"
    err_count=`expr 1 + $err_count`
}
for src in "$@"; do
    p="${src%.ff}.byte"
    echo ${OCAMLBUILD-ocamlbuild} -no-links -quiet $p
    ${OCAMLBUILD-ocamlbuild} -no-links -quiet $p || { fail FC $src; continue; }
    _build/$p || { fail FR $src; continue; }
    echo "OK $p"
done
if [ $err_count -ne 0 ]; then
    echo "$err_count of $# tests failed."
    exit 69
else
    echo "All $* tests succeeded."
fi