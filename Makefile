build:
	opam exec -- opam pin add benchpress.dev https://github.com/sneeuwballen/benchpress.git
	opam exec -- opam install . --deps-only -y
	opam exec -- dune build

clean:
	opam exec -- dune clean

install: build
	opam exec -- opam install . -y

all: build install

doc:
	opam exec -- opam install . --deps-only --with-doc -y
	opam exec -- dune build @doc
