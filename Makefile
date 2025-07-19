.PHONY: all build run clean install

# Nom de l’exécutable
EXEC = main

# Arguments de test par défaut
JSON = machines/unary_sub.json
INPUT = 111111-1111=

all: install build

install:
	opam install --yes dune yojson

build:
	dune build

run: build
	dune exec ./$(EXEC).exe $(JSON) "$(INPUT)"

clean:
	dune clean
