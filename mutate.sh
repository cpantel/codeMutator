BASE=test/php
NAME=EmptyClass
#NAME=SortClass
NAME=SortFunction
NAME=SimpleCode

PRG=$NAME.php

SOURCE=$BASE/$PRG
TOKENS=$BASE/$PRG.json
MUTATIONS=$BASE/mutations/$PRG.mutations.json
OUTPUT=$BASE/mutations/$NAME
# 

echo "==Running php php/tokenize.php $SOURCE $TOKENS"
php php/tokenize.php $SOURCE $TOKENS

RESULT=$?
if [ $RESULT -ne  0 ]; then
  echo "==fail"
  exit 1
fi
echo "==ok"

echo "==Running erl -noshell -s mutator print $TOKENS -s init stop > $MUTATIONS"
# 
# 
erl -noshell -s mutator print $TOKENS -s init stop ;#> $MUTATIONS

RESULT=$?
if [ $RESULT -ne  0 ]; then
  echo fail
  exit 1
fi
echo "==ok"

# 
echo "==Running php php/mutate.php $MUTATIONS $OUTPUT"
php php/mutate.php $MUTATIONS $OUTPUT

RESULT=$?
if [ $RESULT -ne  0 ]; then
  echo fail
  exit 1
fi
echo "==ok"