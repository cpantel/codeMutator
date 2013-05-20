<?php

require_once(dirname(__FILE__). '/Tokenizer.php');
/*
  define classes

  tokenize source OK

  unalias tokens OK

  classify tokens OK

  generate json with classes and tokens
*/


class TokenizerTest extends PHPUnit_Framework_TestCase {
    public function testJson() {
        $t = new Tokenizer();
        $expected = 
        '{"classes":['.
        '{"name":"clone","type":"asymmetric","genes":{"gene":"clone","genePool":["="]}},'.
        '{"name":"flow","type":"asymmetric","genes":{"gene":"exit","genePool":[""]}},'.
        '{"name":"arithmetic","type":"symmetric","genePool":["&","\/","-","%","*","|","+","^"]},'.
        '{"name":"bitwise","type":"symmetric","genePool":[">>","<<"]},'.
        '{"name":"typeCasting","type":"symmetric","genePool":["boolean","integer","double","object","string"]},'.
        '{"name":"logical","type":"symmetric","genePool":["&&","||","and","or","xor"]},'.
        '{"name":"incrementing","type":"symmetric","genePool":["++","--"]},'.
        '{"name":"comparisson","type":"symmetric","genePool":["==",">=","===","!=","!==","<="]},'.
        '{"name":"accessControl","type":"symmetric","genePool":["public","private","protected"]},'.
        '{"name":"bitwiseAssignment","type":"symmetric","pool":["<<=",">>=","^="]},'.
        '{"name":"assignment","type":"symmetric","pool":["&=",".=","\/=","-=","%=","*=","|=","+="]}'.
        '],"tokens":['.
        '{"class":"inmutable","value":"<?php ","info":1},'.
        '{"class":"inmutable","value":"$a","info":1},'.
        '{"class":"assignment","value":"=","info":0},'.
        '{"class":"inmutable","value":"1","info":1},'.
        '{"class":"inmutable","value":";","info":0},'.
        '{"class":"inmutable","value":"$a","info":1},'.
        '{"class":"assignment","value":"=","info":0},'.
        '{"class":"inmutable","value":"$a","info":1},'.
        '{"class":"arithmetic","value":"+","info":0},'.
        '{"class":"inmutable","value":"1","info":1},'.
        '{"class":"inmutable","value":";","info":0}]}';
        
        $json = $t->toJson($t->tokenize('<?php $a=1;$a=$a+1;'));
        $this->assertEquals($expected, $json);
    }

    
    public function testTokenizeUnalias() {
        $t = new Tokenizer();
        $expected = array(
            array('class'=>'inmutable',   'value'=>'<?php ', 'info'=>1),
            array('class'=>'inmutable',   'value'=>'$a',     'info'=>1),
            array('class'=>'assignment',  'value'=>'=',      'info'=>0),
            array('class'=>'inmutable',   'value'=>'1',      'info'=>1),
            array('class'=>'inmutable',   'value'=>';',      'info'=>0),
            array('class'=>'inmutable',   'value'=>'$a',     'info'=>1),
            array('class'=>'assignment',  'value'=>'=',      'info'=>0),
            array('class'=>'typeCasting', 'value'=>'(int)',  'info'=>1),
            array('class'=>'inmutable',   'value'=>'$a',     'info'=>1),
            array('class'=>'arithmetic',  'value'=>'+',      'info'=>0),
            array('class'=>'inmutable',   'value'=>'1',      'info'=>1),
            array('class'=>'inmutable',   'value'=>';',      'info'=>0),
        );
        $tokenized = $t->tokenize('<?php $a=1;$a=(int)$a+1;');
        
        $this->assertEquals($expected,$tokenized);   
    }
    public function testTokenizeSimple() {
        $t = new Tokenizer();
        $expected = array(
            array('class'=>'inmutable',   'value'=>'<?php ', 'info'=>1),
            array('class'=>'inmutable',   'value'=>'$a',     'info'=>1),
            array('class'=>'assignment',  'value'=>'=',      'info'=>0),
            array('class'=>'inmutable',   'value'=>'1',      'info'=>1),
            array('class'=>'inmutable',   'value'=>';',      'info'=>0),
            array('class'=>'inmutable',   'value'=>'$a',     'info'=>1),
            array('class'=>'assignment',  'value'=>'=',      'info'=>0),
            array('class'=>'inmutable',   'value'=>'$a',     'info'=>1),
            array('class'=>'arithmetic',  'value'=>'+',      'info'=>0),
            array('class'=>'inmutable',   'value'=>'1',      'info'=>1),
            array('class'=>'inmutable',   'value'=>';',      'info'=>0),
        );
        $tokenized = $t->tokenize('<?php $a=1;$a=$a+1;');
        $this->assertEquals($expected,$tokenized);   
    }
    
    public function testTokenizeEmpty() {
        $t = new Tokenizer();
        $expected = array();
        $tokenized = $t->tokenize('');
        $this->assertEquals($expected,$tokenized);   
    }

    public function testClassifyString() {
        $t = new Tokenizer();
        $token = ';';
        $expected = array('class'=>'inmutable','value'=>';', 'info'=>0);
        $classified = $t->classify($token);
        $this->assertEquals($expected,$classified);        

    }
    
    public function testClassifyStringAssignment() {
        $t = new Tokenizer();
        $token = '=';
        $expected = array('class'=>'assignment','value'=>'=', 'info'=>0);
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
