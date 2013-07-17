<?php

$inputFile = $argv[1];
$outputDir = $argv[2];

$mutations = json_decode(file_get_contents($inputFile));
#var_dump($mutations);
$version="aaaaaa";
foreach ($mutations as $mutation) {
   $out = "$outputDir.$version.php";
   $version++;
   print "$out\n";
   $buffer='';
   foreach ($mutation as $token) {
   
       $buffer.= $token->value;
   }
   file_put_contents($out,$buffer);
}
    