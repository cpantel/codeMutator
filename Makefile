mutator.beam: mutator.erl
	erlc +export_all mutator.erl

	
mutator_private_test.beam: mutator_private_test.erl
	erlc mutator_private_test.erl

fixtures.beam: fixtures.erl
	erlc +export_all fixtures.erl

php_test: 
	phpunit --colors php

erl_test: mutator_private_test.beam mutator.beam fixtures.beam
	erl -noshell -s mutator_private_test test -s init stop

full_test: php_test erl_test

clean:
	rm -f *.beam
	cp lib/*.beam .
