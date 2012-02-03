all:
	./rebar get-deps compile

clean:
	./rebar clean

test:
	./rebar skip_deps=true eunit

release:
	./rebar generate -f

edoc:
	./rebar skip_deps=true doc

