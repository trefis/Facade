-include Makefile.config

facade.native:
	ocamlbuild -use-ocamlfind $@

build: facade.native

install: facade.native
	install facade.native $(BIN_DIR)/facade

clean:
	ocamlbuild -clean

.PHONY: clean facade.native install build
