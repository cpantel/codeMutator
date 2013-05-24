helper.beam: helper.erl
	erl -compile helper
        
test: helper.beam
	erl -noshell -s helper test -s init stop
