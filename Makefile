helper.beam: helper.erl
	erlc +export_all helper.erl

	
helper_test.beam: helper_test.erl
	erlc helper_test.erl


php_test: 
	phpunit --colors php

erl_test: helper_test.beam helper.beam 
	erl -noshell -s helper_test test -s init stop

full_test: php_test erl_test

clean:
	rm -f *.beam
	cp lib/*.beam .
