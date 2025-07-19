.PHONY: setup install build run clean

# Nom de l'exécutable
EXEC = main
JSON = machines/unary_sub.json
INPUT = 111111-1111=

# Lance le script d'installation OCaml complet
setup:
	chmod +x install_ocaml.sh
	./install_ocaml.sh

# Installe les dépendances via opam
install:
	opam install --yes dune yojson

# Compile le projet avec dune
build:
	dune build

# Exécute le programme
run: build
	dune exec ./$(EXEC).exe $(JSON) "$(INPUT)"

# Nettoie le projet
clean:
	dune clean