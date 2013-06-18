BASE=test/php
NAME=EmptyClass
#NAME=SortClass
NAME=SimpleCode
NAME=Clone
NAME=DobleClo
NAME=TripleClone
NAME=SortFunction


PRG=$NAME.php

SOURCE=$BASE/$PRG
TOKENS=$BASE/$PRG.json
TEST=$BASE/${NAME}Test.php
MUTATIONS=$BASE/mutations/$PRG.mutations.json
OUTPUT_DIR=$BASE/mutations
OUTPUT_TEMPLATE=$OUTPUT_DIR/$NAME
# 

echo "==Sanity check and timeout sampling"
#phpunit $TEST 
TIMEOUT=2

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


echo "==Checking mutations syntax"
for FILE in $OUTPUT_TEMPLATE* ; do
    php -l $FILE >/dev/null 2>&1
    RESULT=$?
    if [ $RESULT -ne  0 ]; then
       echo WRONG MUTATION $FILE
       rm $FILE
    else 
       echo GOOD MUTATION $FILE
    fi
done



echo "==Running tests"
cp $SOURCE $SOURCE.bak

for FILE in $OUTPUT_TEMPLATE* ; do
  cp $FILE $SOURCE
  timeout $TIMEOUT phpunit $TEST >/dev/null 2>&1
  RESULT=$?
  if [ $RESULT -eq  0 ]; then
     echo "TEST PASS, THATS BAD -- $FILE"
  elif [ $RESULT -eq  124 ]; then
     echo "TEST TIMEOUT, CHECK YOURSELF -- $FILE"
  else
     echo "TEST BROKEN, THATS GOOD -- $FILE"
     rm $FILE
  fi
done

cp $SOURCE.bak $SOURCE

echo "==Checking mutations syntax"
for FILE in $OUTPUT_TEMPLATE* ; do
    echo $FILE
    diff $SOURCE $FILE -y
    echo
done
  


