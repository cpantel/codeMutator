import tokenize
import json


class Tokenizer:
    
    def __init__(self):
        self.tokens=[]
    
    def handle_token(self, type, token, (srow,scol), (erow,ecol), line):
    
        print ">> %d, %d-%d, %d:\t%s\%s" % \
        (srow, scol, erow, ecol, tokenize.tok_name[type], repr(token))
        self.tokens.append((type,token))

    def tokenize(self,fileName):    
        file = open(fileName,'r')    
        tokenize.tokenize(file.readline, self.handle_token)         
        

        
    def untok(self):
        print tokenize.untokenize(self.tokens)      


#t = Tokenizer()
#t.tokenize('src.py')
#t.untok()
