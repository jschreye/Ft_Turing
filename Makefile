all:
	opam install --yes yojson > /dev/null || true
	dune build

run:
	dune exec ./main.exe machines/unary_sub.json "111-11="

clean:
	dune clean