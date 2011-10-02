The Viz Standard Library
========================

This directory contain the sources of the VSL, to be used by the bootstrap
`camlviz` preprocessor and by the final compiler.  ''It is subject to complete
redesign.''

The module hierarchy follows the directory structure.  When compiling to OCaml
using the `camlviz` preprocessor, we use the `mlpack` associated to each
directory.  Also note that the `#?ffoc`-comments are interpreted by `camlviz`
and ignored by the final compiler.

See also `the highlighted VSL sources <http://www.vizlang.org/hlsrc/>`_.
