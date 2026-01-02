.PHONY: all clean doc

build:
	@dune build @install

all: build
	@make -C lib compile_commands.json

test: all
	@dune build @install @runtest

clean:
	@dune clean
	@make -C lib clean-compile-commands

doc:
	@dune build @doc
