<?php
require_once(dirname(__FILE__). '/SortFunction.php');

class SortFunctionTest extends PHPUnit_Framework_TestCase {
   public function testMySwap() {
       $expected=array(3,7);
       $myArray=array(7,3);
       mySwap($myArray,0,1);
       $this->assertEquals($expected,$myArray);

   }
   public function testMySwap2() {
       $expected=array(1,2);
       $myArray=array(2,1);
       mySwap($myArray,0,1);
       $this->assertEquals($expected,$myArray);
   }
}
