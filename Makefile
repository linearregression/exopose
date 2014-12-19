all: deps compile

deps:
	./rebar get-deps

compile: deps
	./rebar compile

clean:
	./rebar clean

test:
	./rebar eunit skip_deps=true
