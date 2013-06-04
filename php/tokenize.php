<?php
require_once(dirname(__FILE__). '/Tokenizer.php');

$inputFile = $argv[1];
$outputFile = $argv[2];
$t = new Tokenizer();
$source = file_get_contents($inputFile);

file_put_contents($outputFile,$t->toJson($t->tokenize($source)));
