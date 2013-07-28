ebin/mutator.beam: core/mutator.erl
	erlc  -o ebin +export_all core/mutator.erl
	
ebin/mutator_private_test.beam: core/test/mutator_private_test.erl
	erlc -o ebin core/test/mutator_private_test.erl

ebin/fixtures.beam: core/test/fixtures.erl
	erlc -o ebin +export_all core/test/fixtures.erl

shunit_test:
	@echo "======== SH TEST ========"
	@echo "no implementado"
	#./modules/test_mutate.sh	

python_test:
	@echo "====== PYTHON TEST ======="
	python modules/python/TestTokenizer.py 2>&1
	#python modules/python/TestRenderer.py 
	
php_test:
	@echo "======== PHP TEST ========"
	phpunit --colors modules/php

erl_test: ebin/mutator_private_test.beam ebin/mutator.beam ebin/fixtures.beam
	@echo "======== ERL TEST ========"
	erl -noshell -pa ebin -pa elib -s mutator_private_test test -s init stop

full_test: php_test erl_test shunit_test python_test

clean:
	rm -f erl_crash.dump
	rm -f ebin/*.beam
	rm -f python/*.pyc
