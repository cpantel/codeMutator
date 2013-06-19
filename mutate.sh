BASE=test/php
NAME=EmptyClass
#NAME=SortClass
NAME=SimpleCode
NAME=Clone
NAME=DobleClo
NAME=TripleClone
NAME=SortFunction
NAME=Timeout

PRG=$NAME.php

SOURCE=$BASE/$PRG
TOKENS=$BASE/$PRG.json
TEST=$BASE/${NAME}Test.php
MUTATIONS=$BASE/mutations/$PRG.mutations.json
OUTPUT_DIR=$BASE/mutations
OUTPUT_TEMPLATE=$OUTPUT_DIR/$NAME
# 

echo "== Sanity check and timeout sampling"
php -l $SOURCE >/dev/null 2>&1
RESULT=$?
if [ $RESULT -ne  0 ]; then
    echo "### Source syntax error: $SOURCE"
    exit 1
fi
echo "=== Source syntax check ok: $SOURCE"

php -l $TEST >/dev/null 2>&1
RESULT=$?
if [ $RESULT -ne  0 ]; then
    echo "### Test syntax error: $TEST"
    exit 1
fi    
echo "=== Test syntax check ok: $SOURCE"

phpunit $TEST 2>/dev/null
RESULT=$?
if [ $RESULT -ne  0 ]; then
    echo "### Reference test failed: $TEST"
    exit 1
fi    
echo "=== Reference test ok: $TEST"

RESPONSE=$(phpunit $TEST | grep -e "Time: .*, Memory:" | cut -d" " -f 2,3)

VALUE=$( echo $RESPONSE | cut -d" " -f 1)
UNIT=$( echo $RESPONSE | cut -d" " -f 2 | cut -b 1)

if [ $VALUE -lt 3  ] ; then
    VALUE=4
else
    VALUE=$(( $VALUE * 2 ))
fi
 
TIMEOUT="${VALUE}${UNIT}"

#TIMEOUT=3 ;#uncomment for testing timeout

echo "=== Timeout: $TIMEOUT"

echo "== Running php php/tokenize.php $SOURCE $TOKENS"
php php/tokenize.php $SOURCE $TOKENS

RESULT=$?
if [ $RESULT -ne  0 ]; then
  echo "### Tokenization failed"
  exit 1
fi
echo "=== OK"

echo "== Running erl -noshell -s mutator print $TOKENS -s init stop > $MUTATIONS"
erl -noshell -s mutator print $TOKENS -s init stop > $MUTATIONS 2>/dev/null

RESULT=$?
if [ $RESULT -ne  0 ]; then
  echo "### FAIL"
  exit 1
fi
echo "=== OK"

# 
echo "== Running php php/mutate.php $MUTATIONS $OUTPUT_TEMPLATE"
php php/mutate.php $MUTATIONS $OUTPUT_TEMPLATE

RESULT=$?
if [ $RESULT -ne  0 ]; then
  echo "### FAIL"
  exit 1
fi
echo "=== OK"


echo "== Checking mutations syntax"
for FILE in $OUTPUT_TEMPLATE* ; do
    php -l $FILE >/dev/null 2>&1
    RESULT=$?
    if [ $RESULT -ne  0 ]; then
       echo "--- WRONG MUTATION $FILE"
       rm $FILE
    else 
       echo "--- GOOD MUTATION $FILE"
    fi
done


echo "== Running tests"
cp $SOURCE $SOURCE.bak

for FILE in $OUTPUT_TEMPLATE* ; do
  cp $FILE $SOURCE
  timeout "$TIMEOUT" phpunit $TEST >/dev/null 2>&1
  RESULT=$?
  if [ $RESULT -eq  0 ]; then
     echo "--- TEST PASS, THATS BAD -- $FILE"
  elif [ $RESULT -eq  124 ]; then
     echo "--- TEST TIMEOUT, CHECK YOURSELF -- $FILE"
  else
     echo "--- TEST BROKEN, THATS GOOD -- $FILE"
     rm $FILE
  fi
done

cp $SOURCE.bak $SOURCE

echo "== Checking mutations syntax"
for FILE in $OUTPUT_TEMPLATE* ; do
    echo "--- $FILE"
    diff $SOURCE $FILE -y
    echo
done
  


