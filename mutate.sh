
SOURCE=php/Tokenizer.php
TOKENS=mutations/Tokenizer.php.json
MUTATIONS=mutations/Tokenizer.php.mutations.json
OUTPUT=mutations/dump

php php/tokenize.php $INPUT $TOKENS


erl -noshell -s mutator print $TOKENS -s init stop > $MUTATIONS


php php/mutate.php $MUTATIONS $OUTPUT