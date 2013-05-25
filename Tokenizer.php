<?php

class Tokenizer {
    private $aliases = array(
        '<>'      => '!=',
        '(bool)'  => '(boolean)',
        '(int)'   => '(integer)',
        '(real)'  => '(double)',
        '(float)' => '(double)',
    );

    
    private $classDescription = array(
        array(
          'name'=>'clone',
          'type'=>'asymmetric',
          'genes'=>array(
            array('gene'=>'clone', 'genePool'=>array('='))
          )
        ),
        array(
          'name'=>'flow',
          'type'=>'asymmetric',
          'genes'=> array(
            array('gene'=>'break','genePool' => array('','continue','exit','return')),
            array('gene'=>'return','genePool'=> array('','break','continue','exit')),
            array('gene'=>'continue','genePool'=> array('','break','exit','return')),
            array('gene'=>'exit','genePool'=> array(''))
          )  
        ),
        array(
          'name'=>'arithmetic',
          'type'=>'symmetric',
          'genePool'=>array('&','/','-','%','*','|','+','^')
        ),
        array(
          'name'=>'bitwise',
          'type'=>'symmetric',
          'genePool'=>array('>>','<<')
        ),
        array(
          'name'=>'typeCasting',
          'type'=>'symmetric',
          'genePool'=>array('boolean','integer', 'double', 'object', 'string')
        ),
        array(
          'name'=>'logical',
          'type'=>'symmetric',
          'genePool'=>array('&&','||','and','or','xor')
        ),
        array(
          'name'=>'incrementing',
          'type'=>'symmetric',
          'genePool'=>array('++','--')
        ),
        array(
          'name'=>'comparisson',
          'type'=>'symmetric',
          'genePool'=>array('==','>=','===','!=','!==','<=')
        ),
        array(
          'name'=>'accessControl',
          'type'=>'symmetric',
          'genePool'=>array('public','private','protected')
        ),
        array(
          'name'=>'bitwiseAssignment',
          'type'=>'symmetric',
          'genePool'=>array('<<=','>>=','^=')
        ),
        array(
          'name'=>'assignment',
          'type'=>'symmetric',
          'genePool'=>array('&=','.=','/=','-=','%=','*=','|=','+=')
        ),
    );
    
    private $classMapper = array(
        T_AND_EQUAL => 'assignment',
        T_CONCAT_EQUAL => 'assignment',    
        T_DIV_EQUAL => 'assignment',
        T_MINUS_EQUAL => 'assignment',
        T_MOD_EQUAL => 'assignment', 
        T_MUL_EQUAL => 'assignment',
        T_OR_EQUAL => 'assignment',
        T_PLUS_EQUAL => 'assignment',
        T_IS_EQUAL => 'comparisson',
        T_IS_GREATER_OR_EQUAL => 'comparisson',
        T_IS_IDENTICAL => 'comparisson',
        T_IS_NOT_EQUAL => 'comparisson',
        T_IS_NOT_IDENTICAL => 'comparisson',   
        T_IS_SMALLER_OR_EQUAL => 'comparisson',
        T_BOOLEAN_AND => 'logical',
        T_BOOLEAN_OR => 'logical',
        T_LOGICAL_AND => 'logical',    
        T_LOGICAL_OR => 'logical',
        T_LOGICAL_XOR => 'logical',
        T_PRIVATE => 'accessControl', 
        T_PROTECTED => 'accessControl',
        T_PUBLIC => 'accessControl',
        T_DEC => 'incrementing ',    
        T_INC => 'incrementing ',
        T_SL_EQUAL => 'bitwiseAssignment',
        T_SR_EQUAL => 'bitwiseAssignment', 
        T_XOR_EQUAL => 'bitwiseAssignment',
        T_BOOL_CAST => 'typeCasting',
        T_DOUBLE_CAST => 'typeCasting',
        T_INT_CAST => 'typeCasting',
        T_OBJECT_CAST => 'typeCasting',
        T_STRING_CAST => 'typeCasting',
        T_SL => 'bitwise', 
        T_SR => 'bitwise',
        T_CLONE    => 'clone',
        T_BREAK => 'flow',    
        T_EXIT => 'flow',
        T_CONTINUE => 'flow',
        T_RETURN => 'flow', 
    );
    
    public function toJson($tokens){
        return json_encode(array('classes'=>$this->classDescription, 'tokens'=>$tokens));
    }
    
    public function tokenize($source){
        $tokens = array();
        foreach(token_get_all($source) as $token) {
            $new_token = $this->classify($token);
            $tokens[]=$new_token;
        }
        return $tokens;
    }
    
    public function classify($token) {
        if (is_array($token)) {
            if (isset($this->classMapper[$token[0]])) {
                return array('class'=>$this->classMapper[$token[0]], 'value'=>$token[1], 'info'=>$token[2]);  
            }
            return array('class'=>'string', 'value'=>$token[1], 'info'=>$token[2]);
        } else {
            if ($token == '=') {
                return array('class'=>'assignment', 'value'=>'=', 'info'=>0);
            }
            if (strpos('&/-%*|+^',$token) === false) {
                return array('class'=>'string', 'value'=>$token, 'info'=>0);
            }
            return array('class'=>'arithmetic', 'value'=>$token, 'info'=>0);
        }
    }
    
    public function unalias($token) {
       if (is_array($token) && isset($this->aliases[$token[1]])) {
           $token[1]=$this->aliases[$token[1]];
       }
       return $token;
    }
}
