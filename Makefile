OCAMLBUILD = ocamlbuild

all:
	$(OCAMLBUILD) fflib.otarget

check:
	$(OCAMLBUILD) tests/test.byte -- -verbose

doc:
	$(OCAMLBUILD) ffoc1.docdir/index.html

install:
	@echo 'There is no production compiler to install yet, but you can'
	@echo 'create a symbolic link to bin/ffoc1pp or add the bin/ dir to'
	@echo 'your $$PATH for testing.'

.PHONY: all check doc install
