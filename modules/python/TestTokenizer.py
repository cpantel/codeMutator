import unittest
from collections import OrderedDict

class TestTokenizer(unittest.TestCase):
    def setUp(self):
        pass

    def test_stringConstructor(self):
        tok = Tokenizer.newFromString("a = 1 + 2")

    def test_classify(self):
        expected = [
            OrderedDict([('class', 'NAME'), ('value', "'a'"), ('info', [1, 0, 1, 1])]),
            OrderedDict([('class', 'OP'), ('value', "'='"), ('info', [1, 2, 1, 3])]),
            OrderedDict([('class', 'NUMBER'), ('value', "'1'"), ('info', [1, 4, 1, 5])]),
            OrderedDict([('class', 'arithmetic'), ('value', "'+'"), ('info', [1, 6, 1, 7])]),
            OrderedDict([('class', 'NUMBER'), ('value', "'2'"), ('info', [1, 8, 1, 9])]),
            OrderedDict([('class', 'ENDMARKER'), ('value', "''"), ('info', [2, 0, 2, 0])])
        ]

        tok = Tokenizer.newFromString("a = 1 + 2")
        tok.tokenize()
        tok.classify()
        self.assertEquals(expected, tok.tokens)
        
    def test_fileConstructor(self):
        tok = Tokenizer.newFromFile('test/python/Code.py')

    def test_buildMapper(self):
        tok = Tokenizer.newFromString("")

        self.assertEquals('arithmetic',tok.opMapper["'+'"])

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

    def test_string_tokensToJson(self):
        expected = '[{"class": "NAME", "value": "\'a\'", "info": [1, 0, 1, 1]}, {"class": "OP", "value": "\'=\'", "info": [1, 2, 1, 3]}, {"class": "NUMBER", "value": "\'1\'", "info": [1, 4, 1, 5]}, {"class": "arithmetic", "value": "\'+\'", "info": [1, 6, 1, 7]}, {"class": "NUMBER", "value": "\'2\'", "info": [1, 8, 1, 9]}, {"class": "ENDMARKER", "value": "\'\'", "info": [2, 0, 2, 0]}]'
        tok = Tokenizer.newFromString("a = 1 + 2")
        tok.tokenize()
        tok.classify()
        got = tok.tokensToJson()
        self.assertEquals(expected, got)

    def test_file_tokensToJson(self):
        expected = '[{"class": "NAME", "value": "\'a\'", "info": [1, 0, 1, 1]}, {"class": "OP", "value": "\'=\'", "info": [1, 2, 1, 3]}, {"class": "NUMBER", "value": "\'1\'", "info": [1, 4, 1, 5]}, {"class": "arithmetic", "value": "\'+\'", "info": [1, 6, 1, 7]}, {"class": "NUMBER", "value": "\'2\'", "info": [1, 8, 1, 9]}, {"class": "ENDMARKER", "value": "\'\'", "info": [2, 0, 2, 0]}]'
        tok = Tokenizer.newFromFile('test/python/Code.py')
        tok.tokenize()
        tok.classify()
        got = tok.tokensToJson()
        self.assertEquals(expected, got)

    def test_dump(self):
        expected = '{"classes": [{"name": "NUMBER", "type": "inmutable", "genes": []}, {"name": "ENDMARKER", "type": "inmutable", "genes": []}, {"name": "NAME", "type": "inmutable", "genes": []}, {"name": "OP", "type": "inmutable", "genes": []}, {"name": "NEWLINE", "type": "inmutable", "genes": []}, {"name": "INDENT", "type": "inmutable", "genes": []}, {"name": "DEDENT", "type": "inmutable", "genes": []}, {"name": "NL", "type": "inmutable", "genes": []}, {"name": "COMMENT", "type": "inmutable", "genes": []}, {"name": "STRING", "type": "inmutable", "genes": []}, {"name": "arithmetic", "type": "symmetric", "genePool": ["/", "-", "*", "+"]}, {"name": "logical", "type": "symmetric", "genePool": ["and", "or"]}], "tokens": [{"class": "NAME", "value": "\'a\'", "info": [1, 0, 1, 1]}, {"class": "OP", "value": "\'=\'", "info": [1, 2, 1, 3]}, {"class": "NUMBER", "value": "\'1\'", "info": [1, 4, 1, 5]}, {"class": "arithmetic", "value": "\'+\'", "info": [1, 6, 1, 7]}, {"class": "NUMBER", "value": "\'2\'", "info": [1, 8, 1, 9]}, {"class": "ENDMARKER", "value": "\'\'", "info": [2, 0, 2, 0]}]}'

        tok = Tokenizer.newFromString("a = 1 + 32")
        got = tok.dump()
        self.assertEquals(expected, got)
 


#    de ftest_classify(self):
#        tok = Tokenizer.newFromString(code)


        
if __name__ == '__main__':
    from Tokenizer import Tokenizer
    unittest.main()
