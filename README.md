# The Viz Source Distribution

This project is experimental and only sporadically updated.

## About

Viz is a statically typed, strict, and pure functional programming language
related to the ML family, and to some extent to Haskell.  Both the design
and the implementation is under development.  At the moment, a subset of the
language is implemented as an OCaml preprocessor.  This will later be used
to bootstrap a full stand-alone implementation.

See also:

* [The Viz Language Site](http://www.vizlang.org/)
* [The GitHub Wiki](https://github.com/paurkedal/viz/wiki.html)

## Quick Start

The following packages are required:

* The OCaml complier.
* The ocamlfind utility (findlib).
* The Menhir parser generator.
* The camomile and sexplib libraries.

You can then check out and build the preprocessor
```sh
git checkout git@github.com:paurkedal/viz.git
cd viz
make
```
There is nothing to install at the moment due to the early development
stage.  You may run example programs with either of the commands
```sh
ocamlbuild examples/PROGNAME.byte --
ocamlbuild examples/PROGNAME.native --
```
A convenience wrapper is available for compiling code outside the
repository.  To use it add the bin subdirectory to your $PATH, or create a
link
```sh
ln -s $VIZ_SRCDIR/bin/vizdev $HOME/bin/camlviz
```
or to somewhere else in your $PATH.  You can now compile programs with
```sh
camlviz ocamlc -o foo foo.viz
```
or
```sh
camlviz ocamlopt -c foo.viz
camlviz ocamlopt foo.cmx -o foo
```

## Directories

* `bin` - Program source code and wrapper scripts.
* `camlviz` - The internal library used to implement the OCaml preprocessor
  and related utilities named `bin/camlviz*`.
* `camlviz-tests` - OUnit tests for camlviz.
* `vsl` - The Viz Standard Library.
* `examples` - Viz example programs.
* `tests` - Viz tests.
