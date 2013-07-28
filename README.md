As soon as you clone, follow these steps:

 mkdir elib
 
 cd elib
 
 git clone https://github.com/jchris/erlang-json-eep-parser.git

 cd erlang-json-eep-parser
 
 erlc json_eep.erl 
 
 erlc json_grammar.erl 
 
 erlc json_lex2.erl 
 
 mv *beam ..

 cd ../..
 
 # rm -rf erlang-json-eep-parser

 make full_test

shunit tests are not implemented yet.


More info at http://seguridad-agile.blogspot.com/2013/07/mutation-testing-framework.html

