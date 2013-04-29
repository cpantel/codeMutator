<?php

require_once(dirname(__FILE__). '/Tokenizer.php');
/*
  define classes

  tokenize source

  unalias tokens

  classify tokens

  normalize tokens

  generate json with classes and tokens
*/

class TokenizerTest extends PHPUnit_Framework_TestCase {

    public function testClassifyString() {
        $t = new Tokenizer();
        $token = ';';
        $expected = array('class'=>'inmutable','value'=>';', 'info'=>0);
        $classified = $t->classify($token);
        $this->assertEquals($expected,$classified);        

    }

    public function testClassifyStringArithmetic() {
        $t = new Tokenizer();
        $token = '+';
        $expected = array('class'=>'arithmetic','value'=>'+', 'info'=>0);
        $classified = $t->classify($token);
        $this->assertEquals($expected,$classified);        

    }
    
    public function testClassifyTokenInmutableToken() {
        $t = new Tokenizer();
        $token = array(0, " ", 1);
        $expected = array('class'=>'inmutable','value'=>' ', 'info'=>1);
        $classified = $t->classify($token);
        $this->assertEquals($expected,$classified);        
    }     
    
    public function testClassifyToken() {
        $t = new Tokenizer();
        $token = array(282, "!=", 1);
        $expected = array('class'=>'comparisson','value'=>'!=', 'info'=>1);

        $classified = $t->classify($token);
        $this->assertEquals($expected,$classified);     
    }

    
    public function testUnaliasNotEquals() {
        $t = new Tokenizer();
        $token = array(282, "!=", 1);

        $unaliased = $t->unalias($token);
        $this->assertEquals("!=", $unaliased[1]);
    }
    
    public function testUnaliasDistinct() {
        $t = new Tokenizer();
        $token = array(282, "<>", 1);

        $unaliased = $t->unalias($token);
        $this->assertEquals("!=", $unaliased[1]);
    }
    
    public function testUnaliasString() {
        $t = new Tokenizer();
        $unaliased = $t->unalias(';');
        $this->assertEquals(";", $unaliased);
    }
}
