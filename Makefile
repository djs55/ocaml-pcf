all: build

setup.data: _oasis
	ocaml setup.ml -configure

.PHONY: build
build: setup.data
	ocaml setup.ml -build

install: build
	ocaml setup.ml -install

.PHONY: clean
clean:
	rm -rf _build
