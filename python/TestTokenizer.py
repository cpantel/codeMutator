from Tokenizer import Tokenizer
import unittest
from collections import OrderedDict

class TestTokenizer(unittest.TestCase):
    def setUp(self):
        pass
    def test_stringConstructor(self):
        code =  "class SortClass:\n" +\
        "  def __init__(self, list):\n" +\
        "    self.list=list\n" +\
        "  def mySwap(self,p1,p2):\n" +\
        "    tmp = self.list[p1]\n" +\
        "    self.list[p1]=self.list[p2]\n" +\
        "    self.list[p2]=tmp\n" +\
        "  def mySort(self):\n" +\
        "    if (self.list[0]>self.list[1]):\n" +\
        "      self.mySwap(self.list,0,1)\n"
        tok = Tokenizer.newFromString(code)
        
    def test_fileConstructor(self):
        tok = Tokenizer.newFromFile('test/python/Code.py')

    def test_file_tokenize(self):
        expected = [OrderedDict([('class', 'NAME'), ('value', "'a'"), ('info', [1, 0, 1, 1])]), OrderedDict([('class', 'OP'), ('value', "'='"), ('info', [1, 2, 1, 3])]), OrderedDict([('class', 'NUMBER'), ('value', "'1'"), ('info', [1, 4, 1, 5])]), OrderedDict([('class', 'OP'), ('value', "'+'"), ('info', [1, 6, 1, 7])]), OrderedDict([('class', 'NUMBER'), ('value', "'2'"), ('info', [1, 8, 1, 9])]), OrderedDict([('class', 'ENDMARKER'), ('value', "''"), ('info', [2, 0, 2, 0])])]
        tok = Tokenizer.newFromFile('test/python/Code.py')
        got = tok.tokenize()
        self.assertEquals(expected,got)
        
    def test_simple_tokenize2(self):
        lastExpected = OrderedDict([('class', 'ENDMARKER'), ('value', "''"), ('info', [2, 0, 2, 0])])
        firstExpected = OrderedDict([('class', 'NAME'), ('value', "'a'"), ('info', [1, 0, 1, 1])])
        tok = Tokenizer.newFromString("a = 1 + 2")
        tokens= tok.tokenize()
        firstToken = tokens[0]
        lastToken = tokens[len(tokens) -1]
        self.assertEquals(firstExpected,firstToken)
        self.assertEquals(lastExpected,lastToken)

    def test_string_toJson(self):
        expected = '[{"class": "NAME", "value": "\'a\'", "info": [1, 0, 1, 1]}, {"class": "OP", "value": "\'=\'", "info": [1, 2, 1, 3]}, {"class": "NUMBER", "value": "\'1\'", "info": [1, 4, 1, 5]}, {"class": "OP", "value": "\'+\'", "info": [1, 6, 1, 7]}, {"class": "NUMBER", "value": "\'2\'", "info": [1, 8, 1, 9]}, {"class": "ENDMARKER", "value": "\'\'", "info": [2, 0, 2, 0]}]'
        tok = Tokenizer.newFromString("a = 1 + 2")
        tok.tokenize()
        got = tok.toJson()
        self.assertEquals(expected, got)

    def test_file_toJson(self):
        expected = '[{"class": "NAME", "value": "\'a\'", "info": [1, 0, 1, 1]}, {"class": "OP", "value": "\'=\'", "info": [1, 2, 1, 3]}, {"class": "NUMBER", "value": "\'1\'", "info": [1, 4, 1, 5]}, {"class": "OP", "value": "\'+\'", "info": [1, 6, 1, 7]}, {"class": "NUMBER", "value": "\'2\'", "info": [1, 8, 1, 9]}, {"class": "ENDMARKER", "value": "\'\'", "info": [2, 0, 2, 0]}]'
        tok = Tokenizer.newFromFile('test/python/Code.py')
        tok.tokenize()
        got = tok.toJson()
        self.assertEquals(expected, got)
        
if __name__ == '__main__':
    unittest.main()
