DIR=$(basename $(dirname $0))

if [ -e "${PWD}/${DIR}/mutate.sh" ]; then
    . "${PWD}/${DIR}/mutate.sh"
else
    echo not sourcing "${PWD}/${DIR}/mutate.sh"
    exit 1
fi

# check quotes in every variable
BASE=$(pwd)/test/python


NAME="$1"


PRG=$NAME.py

SOURCE=$BASE/$PRG
TOKENS=$BASE/mutations/$PRG.seed.json
TEST=$BASE/Test${NAME}.py
MUTATIONS=$BASE/mutations/$PRG.mutations.json
OUTPUT_DIR=$BASE/mutations
OUTPUT_TEMPLATE=$OUTPUT_DIR/$NAME
REPORT=$OUTPUT_DIR/report.txt
STATS_TIME_START=$( date "+%s" )

prepareWorkspace $OUTPUT_DIR

echo "== Sanity check and timeout sampling"
python -m py_compile $SOURCE >/dev/null 2>&1
checkResult "### Source syntax error: $SOURCE" 1 "=== Source syntax check ok: $SOURCE"

python -m py_compile $TEST >/dev/null 2>&1

checkResult "### Test syntax error: $TEST" 1 "=== Test syntax check ok: $TEST"

python $TEST 1>/dev/null 2>&1

checkResult "### Reference test failed: $TEST" 1  "=== Reference test ok: $TEST"

RESPONSE=$(python $TEST  2>&1 | grep -e "Ran .* tests" | cut -d" " -f 5)

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

debug "== Running  python modules/python/tokenizeIt.py $SOURCE $TOKENS"
python modules/python/tokenizeIt.py $SOURCE $TOKENS

checkResult "Tokenization failed"

debug "== Running erl -noshell  -pa ebin -pa elib -s mutator print $TOKENS -s init stop > $MUTATIONS"
erl -noshell -pa ebin -pa elib -s mutator print $TOKENS -s init stop > $MUTATIONS 2>/dev/null

checkResult "### Mutation pool failed" 1 "=== Mutation pool OK"

debug "== Running python modules/python/render.py $MUTATIONS $OUTPUT_TEMPLATE"
python modules/python/renderIt.py $MUTATIONS $OUTPUT_TEMPLATE | while read LINE; do
    echo -n "."
done
echo
 
checkResult  "### Mutation rendering FAIL" 1 "=== Mutation rendering OK"

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
    python -m py_compile $FILE >/dev/null 2>&1
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
  rm ${SOURCE}c
  timeout "$TIMEOUT" python $TEST >/dev/null 2>&1
  RESULT=$?
  if [ $RESULT -eq  0 ]; then
     echo "--- TEST PASS, THATS BAD -- $FILE"
     STATS_SURVIVORS=$(( $STATS_SURVIVORS + 1 ))
     DIFFS="${DIFFS}\ndiff -w $SOURCE $FILE ;#SURVIVOR"
  elif [ $RESULT -eq  124 ]; then
     echo "--- TEST TIMEOUT, CHECK YOURSELF -- $FILE"
     STATS_TIMEOUTS=$(( $STATS_TIMEOUTS + 1 ))
     DIFFS="${DIFFS}\ndiff -w $SOURCE $FILE ;#TIMEOUT"
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
