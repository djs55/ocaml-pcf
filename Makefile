all: build

setup.data: _oasis
	ocaml setup.ml -configure

.PHONY: build
build: setup.data
	ocaml setup.ml -build

install: build
	ocamlfind remove pcf
	ocaml setup.ml -install

.PHONY: clean
clean:
	rm -rf _build setup.data
