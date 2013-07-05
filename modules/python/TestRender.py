from Render import Render
import unittest
from collections import OrderedDict

class TestRender(unittest.TestCase):
    def setUp(self):
        pass

    def test_stringConstructor(self):
        render = Render.newFromString("a = 1 + 2")

        
    def test_fileConstructor(self):
        render = Render.newFromFile('test/Code.json')


        
if __name__ == '__main__':
    unittest.main()
