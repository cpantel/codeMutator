#! /bin/bash
function testPhp() {
    EXPECTED="SOURCE /home/carlos/proyectos/codeMutator/test/php/SortFunction.php
TEST /home/carlos/proyectos/codeMutator/test/php/SortFunctionTest.php
LINES 13
STATS_TOTAL_MUTATIONS 39
STATS_WRONG_MUTATIONS 14
STATS_GOOD_MUTATIONS 25
STATS_DEADS 24
STATS_SURVIVORS 1

diff /home/carlos/proyectos/codeMutator/test/php/SortFunction.php /home/carlos/proyectos/codeMutator/test/php/mutations/SortFunction.aaa.php ;#SURVIVOR"
    ./modules/mutate_php.sh SortFunction >/dev/null 2>&1 
   
    RESULT=$(grep -ve TIMEOUT -ve TIME.*seconds ./test/php/mutations/report.txt)

    assertEquals "$EXPECTED" "$RESULT"

}

function testPython() {
    EXPECTED="SOURCE /home/carlos/proyectos/codeMutator/test/python/SortFunction.py
TEST /home/carlos/proyectos/codeMutator/test/python/SortFunctionTest.py
LINES 
STATS_TOTAL_MUTATIONS 
STATS_WRONG_MUTATIONS 
STATS_GOOD_MUTATIONS 
STATS_DEADS 
STATS_SURVIVORS 

diff /home/carlos/proyectos/codeMutator/test/php/SortFunction.php /home/carlos/proyectos/codeMutator/test/php/mutations/SortFunction.aaa.php ;#SURVIVOR"
    ./modules/mutate_py.sh SortFunction ; #>/dev/null 2>&1 

    RESULT=$(grep -ve TIMEOUT -ve TIME.*seconds ./test/python/mutations/report.txt)

    assertEquals "$EXPECTED" "$RESULT"

}



. shunit2
