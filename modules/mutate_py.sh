function output {
    echo $* >> $REPORT
    echo $*
}

# check quotes in every variable
BASE=$(pwd)/test/python


NAME="$1"


PRG=$NAME.py

SOURCE=$BASE/$PRG
TOKENS=$BASE/$PRG.json
TEST=$BASE/Test${NAME}.py
MUTATIONS=$BASE/mutations/$PRG.mutations.json
OUTPUT_DIR=$BASE/mutations
OUTPUT_TEMPLATE=$OUTPUT_DIR/$NAME
REPORT=$OUTPUT_DIR/report.txt
STATS_TIME_START=$( date "+%s" )

echo "== Sanity check and timeout sampling"
python -m py_compile $SOURCE >/dev/null 2>&1
RESULT=$?
if [ $RESULT -ne  0 ]; then
    echo "### Source syntax error: $SOURCE"
    exit 1
fi
echo "=== Source syntax check ok: $SOURCE"

python -m py_compile $TEST >/dev/null 2>&1
RESULT=$?
if [ $RESULT -ne  0 ]; then
    echo "### Test syntax error: $TEST"
    exit 1
fi    
echo "=== Test syntax check ok: $TEST"

python $TEST 1>/dev/null 2>&1
RESULT=$?
if [ $RESULT -ne  0 ]; then
    echo "### Reference test failed: $TEST"
    exit 1
fi    
echo "=== Reference test ok: $TEST"



RESPONSE=$(python $TEST  2>&1 | grep -e "Ran .* tests" | cut -d" " -f 5)
echo $RESPONSE

VALUE=$( echo $RESPONSE | cut -d"." -f 1)
UNIT=s

if [ $VALUE -lt 3  ] ; then
    VALUE=4
else
    VALUE=$(( $VALUE * 2 ))
fi
 
TIMEOUT="${VALUE}${UNIT}"

#TIMEOUT=3 ;#uncomment for testing timeout

echo "=== Timeout: $TIMEOUT"

echo "== Running  python modules/python/tokenizeIt.py $SOURCE $TOKENS"
python modules/python/tokenizeIt.py $SOURCE $TOKENS

RESULT=$?
if [ $RESULT -ne  0 ]; then
  echo "### Tokenization failed"
  exit 1
fi
echo "=== OK"


echo "== Running erl -noshell  -pa ebin -pa elib -s mutator print $TOKENS -s init stop > $MUTATIONS"
erl -noshell -pa ebin -pa elib -s mutator print $TOKENS -s init stop > $MUTATIONS 2>/dev/null

RESULT=$?
if [ $RESULT -ne  0 ]; then
  echo "### FAIL"
  exit 1
fi
echo "=== OK"

# 
echo "== Running python modules/python/render.py $MUTATIONS $OUTPUT_TEMPLATE"
python modules/python/render.py $MUTATIONS $OUTPUT_TEMPLATE | while read LINE; do
    echo -n "."
#    echo "--- $LINE"
done
echo
exit 

RESULT=$?
if [ $RESULT -ne  0 ]; then
  echo "### FAIL"
  exit 1
fi
echo "=== OK"


STATS_LINES=$( wc -l $SOURCE | cut -d" " -f1 )
STATS_TOTAL_MUTATIONS=0
STATS_WRONG_MUTATIONS=0
STATS_GOOD_MUTATIONS=0
STATS_TIMEOUTS=0
STATS_DEADS=0
STATS_SURVIVORS=0
DIFFS=""

echo "== Checking mutations syntax"
for FILE in $OUTPUT_TEMPLATE*.py ; do
    python  $FILE >/dev/null 2>&1
    RESULT=$?
    STATS_TOTAL_MUTATIONS=$(( $STATS_TOTAL_MUTATIONS + 1 ))
    if [ $RESULT -ne  0 ]; then
       echo "--- WRONG MUTATION $FILE"
       rm $FILE
       STATS_WRONG_MUTATIONS=$(( $STATS_WRONG_MUTATIONS + 1 ))
    else 
       echo "--- GOOD MUTATION $FILE"
       STATS_GOOD_MUTATIONS=$(( $STATS_GOOD_MUTATIONS + 1 ))
    fi
done


echo "== Running tests"
cp $SOURCE $SOURCE.bak

for FILE in $OUTPUT_TEMPLATE*.py ; do
  cp $FILE $SOURCE
  timeout "$TIMEOUT" python XXX $TEST >/dev/null 2>&1
  RESULT=$?
  if [ $RESULT -eq  0 ]; then
     echo "--- TEST PASS, THATS BAD -- $FILE"
     STATS_SURVIVORS=$(( $STATS_SURVIVORS + 1 ))
     DIFFS="${DIFFS}\ndiff $SOURCE $FILE ;#SURVIVOR"
  elif [ $RESULT -eq  124 ]; then
     echo "--- TEST TIMEOUT, CHECK YOURSELF -- $FILE"
     STATS_TIMEOUTS=$(( $STATS_TIMEOUTS + 1 ))
     DIFFS="${DIFFS}\ndiff $SOURCE $FILE ;#TIMEOUT"
  else
     echo "--- TEST BROKEN, THATS GOOD -- $FILE"
     rm $FILE
     STATS_DEADS=$(( $STATS_DEADS + 1 ))
  fi
done

cp $SOURCE.bak $SOURCE


STATS_TIME_STOP=$( date "+%s" )
STATS_TIME=$(( $STATS_TIME_STOP - $STATS_TIME_START))

echo
rm -f $REPORT
output "SOURCE                 $SOURCE"
output "TEST                   $TEST"
output "TIMEOUT                $TIMEOUT"
output "LINES                  $STATS_LINES"
output "TIME                   $STATS_TIME seconds"
output "STATS_TOTAL_MUTATIONS  $STATS_TOTAL_MUTATIONS"
output "STATS_WRONG_MUTATIONS  $STATS_WRONG_MUTATIONS"
output "STATS_GOOD_MUTATIONS   $STATS_GOOD_MUTATIONS"
output "STATS_TIMEOUTS         $STATS_TIMEOUTS"
output "STATS_DEADS            $STATS_DEADS"
output "STATS_SURVIVORS        $STATS_SURVIVORS"

output -e "$DIFFS"
