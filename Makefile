OCAMLBUILD = ocamlbuild

all: camlviz-all

byte: camlviz-byte

native: camlviz-native

clean: camlviz-clean

install:
	@echo 'There is no production compiler to install yet.  README.rst'
	@echo 'explains how to the current code nevertheless.'

check: camlviz-check-vsl

.PHONY: all byte native clean check install


# The camlviz Preprocessor and Compatible Build of the Standard Library
# =====================================================================

camlviz_PROGRAMS = bin/camlvizpp bin/camlvizerror
camlviz_BYTE	= $(camlviz_PROGRAMS:%=%.byte) libvsl.a vsl.cma
camlviz_NATIVE	= $(camlviz_PROGRAMS:%=%.native) libvsl.a vsl.cmxa

camlviz-native:
	$(OCAMLBUILD) $(camlviz_NATIVE)
camlviz-byte:
	$(OCAMLBUILD) $(camlviz_BYTE)
camlviz-all:
	$(OCAMLBUILD) $(camlviz_BYTE) $(camlviz_NATIVE)
camlviz-clean:
	$(OCAMLBUILD) -clean

camlviz-check: camlviz-check-camlviz camlviz-check-vsl

camlviz-check-camlviz:
	$(OCAMLBUILD) camlviz-tests/test.byte -- -verbose

camlviz-check-vsl:
	/bin/sh tools/run_tests.sh tests/*.vz

camlviz-doc:
	$(OCAMLBUILD) camlviz.docdir/index.html

.PHONY: camlviz-all camlviz-byte camlviz-native
.PHONY: camlviz-clean camlviz-doc
.PHONY: camlviz-check camlviz-check-camlviz camlviz-check-vsl
