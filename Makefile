all: deps compile

deps:
	./rebar get-deps

compile: deps
	./rebar compile

clean:
	./rebar clean

tests:
	./rebar eunit skip_deps=true
