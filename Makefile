all: prefform-all

clean: prefform-clean

install:
	@echo 'There is no production compiler to install yet.  README.rst'
	@echo 'explains how to the current code nevertheless.'

check: prefform-check

.PHONY: all clean check install

# The ffoc1 Camlp4 Preprocessor and Associated fflib
# ==================================================

OCAMLBUILD = ocamlbuild
PREFFORM_BYTE	= bin/ffoc1pp.byte libfflib.a fflib.cma
PREFFORM_NATIVE	= bin/ffoc1pp.native libfflib.a fflib.cmxa

prefform-native:
	$(OCAMLBUILD) $(PREFFORM_NATIVE)
prefform-byte:
	$(OCAMLBUILD) $(PREFFORM_BYTE)
prefform-all:
	$(OCAMLBUILD) $(PREFFORM_BYTE) $(PREFFORM_NATIVE)
prefform-clean:
	$(OCAMLBUILD) -clean

prefform-check: prefform-check-ffoc1 prefform-check-fflib

prefform-check-ffoc1:
	$(OCAMLBUILD) ffoc1-tests/test.byte -- -verbose

prefform-check-fflib:
	/bin/sh tools/run_tests.sh tests/*.ff

prefform-doc:
	$(OCAMLBUILD) ffoc1.docdir/index.html

.PHONY: prefform-all prefform-byte prefform-native
.PHONY: prefform-clean prefform-doc
.PHONY: prefform-check prefform-check-ffoc1 prefform-check-fflib
