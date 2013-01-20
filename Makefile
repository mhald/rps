all: compile

compile:
	@./rebar compile

doc:
	@mkdir -p doc

docs:	doc doc/*.html

doc/*.html:
	@erl -eval 'edoc:files(["./src/gen_nb_server.erl"])' -noshell -s init stop
	@mv *.html erlang.png stylesheet.css edoc-info doc

clean:
	@rm -rf doc
	@./rebar clean

shell: compile
	erl -pa ebin -smp enable -boot start_sasl -sname stats 
