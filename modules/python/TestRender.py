from Renderer import Renderer
import unittest
from collections import OrderedDict

class TestRender(unittest.TestCase):
    def setUp(self):
        pass

    def test_stringConstructor(self):
        render = Renderer.newFromString("a = 1 + 2")

        
    def test_fileConstructor(self):
        render = Renderer.newFromFile('modules/python/test/Code.json')


        
if __name__ == '__main__':
    unittest.main()
