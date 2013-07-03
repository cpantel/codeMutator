<?php

require_once(dirname(__FILE__). '/Tokenizer.php');

$t = new Tokenizer();


print($t->toJson($t->tokenize()));
