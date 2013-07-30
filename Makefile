all: build

setup.data: _oasis
	ocaml setup.ml -configure

build: setup.data
	ocaml setup.ml -build

.PHONY: clean
clean:
	rm -rf _build
