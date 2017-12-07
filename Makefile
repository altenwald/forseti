REBAR=./rebar3

all: compile

clean:
	${REBAR} clean
	-rm -rf _build

doc:
	${REBAR} as doc edoc

compile:
	${REBAR} compile

test:
	${REBAR} do eunit, cover
	./covertool \
		-cover .eunit/eunit.coverdata \
		-appname forseti \
		-output cobertura.xml
	-rm -f forseti_leader_forseti*
	-rm -rf Mnesia.forseti*

.PHONY: doc test compile all
