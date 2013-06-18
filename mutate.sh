BASE=test/php
NAME=EmptyClass
#NAME=SortClass
NAME=SortFunction
NAME=SimpleCode
NAME=Clone
NAME=DobleClo
NAME=TripleClone



PRG=$NAME.php

SOURCE=$BASE/$PRG
TOKENS=$BASE/$PRG.json
MUTATIONS=$BASE/mutations/$PRG.mutations.json
OUTPUT_DIR=$BASE/mutations
OUTPUT_TEMPLATE=$OUTPUT_DIR/$NAME
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
erl -noshell -s mutator print $TOKENS -s init stop > $MUTATIONS 2>/dev/null

RESULT=$?
if [ $RESULT -ne  0 ]; then
  echo fail
  exit 1
fi
echo "==ok"

# 
echo "==Running php php/mutate.php $MUTATIONS $OUTPUT_TEMPLATE"
php php/mutate.php $MUTATIONS $OUTPUT_TEMPLATE

RESULT=$?
if [ $RESULT -ne  0 ]; then
  echo fail
  exit 1
fi
echo "==ok"

echo "checking mutations sintax"
for FILE in $OUTPUT_TEMPLATE* ; do
    php -l $FILE >/dev/null 2>&1
    RESULT=$?
    if [ $RESULT -ne  0 ]; then
       echo WRONG MUTATION $FILE
    else 
       echo GOOD MUTATION $FILE
    fi
done
