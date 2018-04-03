SRC=$(wildcard src/*.mli src/*.ml src/*.mly src/*.mll)

all: test.nb

build: build.timestamp

build.timestamp: $(SRC) src/jbuild
	jbuilder build src/test.exe
	ln -s _build/default/src/test.exe .
	touch build.timestamp

test.nb: build.timestamp test.m
	./test.exe test.m > test.nb

clean:
	rm -f build.timestamp test.exe

.PHONY: build all clean