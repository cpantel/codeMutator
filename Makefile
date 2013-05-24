helper.beam: helper.erl
	erl -compile helper.erl

	
helper_test.beam: helper_test.erl
	erl -compile helper_test


clean:
	rm *.erl

test: helper_test.beam helper.beam
	erl -noshell -s helper_test test -s init stop
