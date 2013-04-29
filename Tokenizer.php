<?php

class Tokenizer {
    private $aliases = array(
        '<>'      => '!=',
        '(bool)'  => '(boolean)',
        '(int)'   => '(integer)',
        '(real)'  => '(double)',
        '(float)' => '(double)',
    );
    
    private $classes = array(
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
        T_SL_EQUAL => 'bitwiseassignment',
        T_SR_EQUAL => 'bitwiseassignment', 
        T_XOR_EQUAL => 'bitwiseassignment',
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
    
    public function classify($token) {
        if (is_array($token)) {
            if (isset($this->classes[$token[0]])) {
                return array('class'=>$this->classes[$token[0]], 'value'=>$token[1], 'info'=>$token[2]);  
            }
            return array('class'=>'inmutable', 'value'=>$token[1], 'info'=>$token[2]);
        } else {
            if (strpos('&/-%*|+^',$token) === false) {
                return array('class'=>'inmutable', 'value'=>$token, 'info'=>0);
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
