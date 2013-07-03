ebin/mutator.beam: mutator.erl
	erlc +export_all mutator.erl -o ebin/mutator.beam
	
ebin/mutator_private_test.beam: mutator_private_test.erl
	erlc mutator_private_test.erl -o mutator_private_test.beam

ebin/fixtures.beam: fixtures.erl
	erlc +export_all fixtures.erl -o fixtures.beam

shunit_test:
	@echo "======== SH TEST ========"
	@echo "TBI"

python_test:
	@echo "====== PYTHON TEST ======="
	python python/TestTokenizer.py 2>&1
	
php_test:
	@echo "======== PHP TEST ========"
	phpunit --colors php

erl_test: ebin/mutator_private_test.beam ebin/mutator.beam ebin/fixtures.beam
	@echo "======== ERL TEST ========"
	erl -noshell -pa ebin -pa elib -s mutator_private_test test -s init stop

full_test: php_test erl_test shunit_test python_test

clean:
	rm -f ebin/*.beam
	rm -f python/*.pyc
