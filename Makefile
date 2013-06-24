mutator.beam: mutator.erl
	erlc +export_all mutator.erl
	
mutator_private_test.beam: mutator_private_test.erl
	erlc mutator_private_test.erl

fixtures.beam: fixtures.erl
	erlc +export_all fixtures.erl

shunit_test:
	@echo "======== SH TEST ========"
	@echo "TBI"

python_test:
	@echo "====== PYTHON TEST ======="
	python python/TestTokenizer.py 2>&1
	
php_test:
	@echo "======== PHP TEST ========"
	phpunit --colors php

erl_test: mutator_private_test.beam mutator.beam fixtures.beam
	@echo "======== ERL TEST ========"
	erl -noshell -s mutator_private_test test -s init stop

full_test: php_test erl_test shunit_test python_test

clean:
	rm -f *.beam
	cp lib/*.beam .
