===============================
 The Fform Source Distribution
===============================


About
=====

Fform is a statically typed, strict, and pure functional programming language
related to the ML family, and to some extent to Haskell.  Both the design and
the implementation is under development.  At the moment, a subset of the
language is implemented as an O'Caml preprocessor.  This will later be used to
bootstrap a full stand-alone implementation.

See the `Wiki pages`_ for a summary of the planned features.


Quick Start
===========

The following packages are required:

* The O'Caml complier.
* The ocamlfind utility (findlib).
* The Menhir parser generator.
* The camomile and sexplib libraries.

If not available though your package manager, Godi_ is a convenient
alternative.

You can then check out and build the preprocessor::

    git checkout git@github.com:paurkedal/fform.git
    cd fform
    make

There is nothing to install at the moment due to the early development stage.
You may run example programs with either of the commands ::

    ocamlbuild examples/PROGNAME.byte --
    ocamlbuild examples/PROGNAME.native --

A convenience wrapper is available for compiling code outside the repository.
To use it add the bin subdirectory to your $PATH, or create a link ::

    ln -s $FFORM_SRCDIR/bin/ffdev $HOME/bin/prefform

or to somewhere else in your $PATH.  You can now compile programs with ::

    prefform ocamlc -o foo foo.ff

or ::

    prefform ocamlopt -c foo.ff
    prefform ocamlopt foo.cmx -o foo


Directories
===========

* bin - Program source code and wrapper scripts.
* ffoc1 - The O'Caml preprocessor library used by bin/ffoc1pp.
* ffoc1-tests - OUnit tests for ffoc1.
* fflib - The Fform Standard Library.
* examples - Fform example programs.
* tests - Fform tests programs.


.. _Godi: http://godi.camlcity.org/godi/index.html
.. _Wiki pages: https://github.com/paurkedal/fform/wiki.html
