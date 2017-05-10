all: compile

force-deps:
	./rebar get-deps
	./rebar update-deps
	./rebar compile

clean:
	./rebar clean

deps:
	./rebar get-deps
	./rebar compile

doc:
	./rebar doc skip_deps=true

compile: deps
	./rebar compile skip_deps=true

test: deps
	./rebar eunit skip_deps=true
	./covertool \
		-cover .eunit/eunit.coverdata \
		-appname ephp \
		-output cobertura.xml

.PHONY: doc test compile force-deps all

