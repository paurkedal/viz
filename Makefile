OCAMLBUILD = ocamlbuild

all: camlviz-all

byte: camlviz-byte

native: camlviz-native

clean: camlviz-clean

install:
	@echo 'There is no production compiler to install yet.  README.rst'
	@echo 'explains how to the current code nevertheless.'

check: camlviz-check-fflib

.PHONY: all byte native clean check install


# The camlviz Preprocessor and Compatible Build of the Standard Library
# =====================================================================

PREFFORM_BYTE	= bin/camlvizpp.byte libfflib.a fflib.cma
PREFFORM_NATIVE	= bin/camlvizpp.native libfflib.a fflib.cmxa

camlviz-native:
	$(OCAMLBUILD) $(PREFFORM_NATIVE)
camlviz-byte:
	$(OCAMLBUILD) $(PREFFORM_BYTE)
camlviz-all:
	$(OCAMLBUILD) $(PREFFORM_BYTE) $(PREFFORM_NATIVE)
camlviz-clean:
	$(OCAMLBUILD) -clean

camlviz-check: camlviz-check-ffoc1 camlviz-check-fflib

camlviz-check-ffoc1:
	$(OCAMLBUILD) ffoc1-tests/test.byte -- -verbose

camlviz-check-fflib:
	/bin/sh tools/run_tests.sh tests/*.ff

camlviz-doc:
	$(OCAMLBUILD) ffoc1.docdir/index.html

.PHONY: camlviz-all camlviz-byte camlviz-native
.PHONY: camlviz-clean camlviz-doc
.PHONY: camlviz-check camlviz-check-ffoc1 camlviz-check-fflib
