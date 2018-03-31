SRC=$(wildcard src/*.mli src/*.ml src/*.mly src/*.mll)

all: test.json

build: build.timestamp
	echo $(SRC)

build.timestamp: $(SRC) setup.ml
	ocaml setup.ml -build
	touch build.timestamp

test.json: build.timestamp test.m
	./test.byte test.m > test.json