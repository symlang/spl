SRC=$(wildcard src/*.mli src/*.ml src/*.mly src/*.mll)

all: test.nb

build: build.timestamp
	echo $(SRC)

build.timestamp: $(SRC) setup.ml setup.data
	ocaml setup.ml -build
	touch build.timestamp

setup.data: setup.ml
	ocaml setup.ml -configure

setup.ml: _oasis
	oasis setup

test.nb: build.timestamp test.m
	./test.byte test.m > test.nb

clean:
	rm -f build.timestamp setup.data setup.log setup.ml src/*.mldylib src/*.mllib myocamlbuild.ml

.PHONY: build all clean