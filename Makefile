all: hooks build

build:
	rebar3 compile

clean:
	rebar3 clean

lint:
	rebar3 as lint lint

test:
	rebar3 do eunit --cover, cover --verbose

hooks: .git/hooks/pre-commit

.git/hooks/pre-commit:
	echo 'make test' > $@
	chmod +x $@

.PHONY: all build clean test hooks
