import tokenize
import json
import StringIO
from collections import OrderedDict


class Tokenizer:
    
    def __init__(self,source):
        self.tokens=[]
        self.source=source
    
    @classmethod    
    def newFromFile(object,fileName):
        return Tokenizer(open(fileName,'r'))

    @classmethod    
    def newFromString(object,string):
        return Tokenizer(StringIO.StringIO(string))
    
    def handle_token(self, type, token, (srow,scol), (erow,ecol), line):
        entry=OrderedDict([ \
        ('class',tokenize.tok_name[type]), \
        ('value',repr(token)), \
        ('info',[srow,scol,erow,ecol])])
        
        self.tokens.append(entry)

    def tokenize(self):    
        tokenize.tokenize(self.source.readline,  self.handle_token)
        return self.tokens
        
    def classify(self):
        pass
    
   
    def toJson(self):
        return json.dumps(self.tokens)
      
        
    def untok(self):
        print tokenize.untokenize(self.tokens)      


#t = Tokenizer()
#t.tokenize('src.py')
#t.untok()
