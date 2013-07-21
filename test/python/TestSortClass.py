import unittest
from SortClass import SortClass


class TestSortClass(unittest.TestCase):
   def setUp(self):
       pass
    
   def test_MySwap(self):
       expected=[3,7]
       myArray=[7,3]
       sortClass = SortClass(myArray)
       
       sortClass.mySwap(0,1)
       self.assertEquals(expected,myArray)

   def testMySwap2(self):
       expected=[1,2]
       myArray=[2,1]
       sortClass = SortClass(myArray)
       sortClass.mySwap(0,1)
       self.assertEquals(expected,myArray)

   def testMySort(self):
       expected=[1,2]
       myArray=[2,1]
       sortClass = SortClass(myArray)
       sortClass.mySort()
       self.assertEquals(expected,sortClass.list)
       
       
       
if __name__ == '__main__':
    unittest.main()
