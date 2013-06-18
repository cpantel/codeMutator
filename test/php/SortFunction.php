<?php

function mySwap(&$e,$p1,$p2) {
   $tmp = $e[$p1];
   $e[$p1]=$e[$p2];
   $e[$p2]=$tmp;
}

function mySort(&$e) {
   if ($e[0]>$e[1]) {
     mySwap($e,0,1);
   }
}
