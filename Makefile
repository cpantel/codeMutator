helper.beam: helper.erl
	erlc +export_all helper.erl

	
helper_test.beam: helper_test.erl
	erlc helper_test.erl


clean:
	rm -f *.beam
	cp lib/*.beam .

test: helper_test.beam helper.beam 
	erl -noshell -s helper_test test -s init stop
