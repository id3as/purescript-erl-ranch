.PHONY: ps erl all test repl

all: test

ps:
	@spago build

test: 
	@spago -x test.dhall test
