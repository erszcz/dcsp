compile:
	./rebar compile

test: eunit ct

eunit:
	./rebar eunit

ct:
	./rebar ct

console:
	erl -pa ebin/
