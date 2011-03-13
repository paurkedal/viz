OCAMLBUILD = ocamlbuild

BYTE	= bin/ffoc1pp.byte libfflib.a fflib.cma
NATIVE	= bin/ffoc1pp.native libfflib.a fflib.cmxa

native:
	$(OCAMLBUILD) $(NATIVE)
byte:
	$(OCAMLBUILD) $(BYTE)
all:
	$(OCAMLBUILD) $(BYTE) $(NATIVE)
clean:
	$(OCAMLBUILD) -clean

check:
	$(OCAMLBUILD) ffoc1-tests/test.byte -- -verbose

doc:
	$(OCAMLBUILD) ffoc1.docdir/index.html

install:
	@echo 'There is no production compiler to install yet, but you can'
	@echo 'create a symbolic link to bin/ffoc1pp or add the bin/ dir to'
	@echo 'your $$PATH for testing.'

.PHONY: all byte native clean check doc install
