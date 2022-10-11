build:
	opam exec -- opam install . --deps-only
	opam exec -- dune build

clean:
	opam exec -- dune clean

install: build
	opam exec -- opam install .

all: build install

doc:
	opam exec -- opam install . --deps-only --with-doc
	opam exec -- dune build @doc
