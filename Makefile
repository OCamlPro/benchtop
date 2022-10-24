init:
	opam exec -- opam pin add benchpress.dev https://github.com/sneeuwballen/benchpress.git
	opam exec -- opam install . --deps-only -y --working-dir
	
build:
	opam exec -- dune build

clean:
	opam exec -- dune clean

install: build
	opam exec -- opam unpin benchtop
	opam exec -- opam install . -y --working-dir

all: init build install

doc:
	opam exec -- opam install . --deps-only --with-doc -y --working-dir
	opam exec -- dune build @doc

.PHONY: init build clean install all doc
