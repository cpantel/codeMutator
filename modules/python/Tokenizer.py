import tokenize
import json
import StringIO
from collections import OrderedDict


class Tokenizer:

    @classmethod    
    def newFromFile(object,fileName):
        tok = Tokenizer(open(fileName,'r'))
        tok.initAsTokenizer()
        return tok

    @classmethod    
    def newFromString(object,string):
        tok = Tokenizer(StringIO.StringIO(string))
        tok.initAsTokenizer()
        return tok

    #@classmethod    
    #def newRendererFromString(object,string):
        #tok = Tokenizer(StringIO.StringIO(string))
        ##tok.initAsRenderer()
        #return tok

    @classmethod    
    def newRendererFromFile(object,fileName):
        tok = Tokenizer(open(fileName,'r'))
        #tok.initAsRenderer()
        return tok

    def render(self):
        source = self.source
        json16 = json.load(source)
        json8 = self.convert(json16)
        base = 'aaaaaa'
        for file in json8:
            fileName = base + ".py"
            out = open(fileName, 'w')
            tokens=[]
            lineout=""
            indent = 0
            
            for item in file:
                type = item.get('info')[0]
                value = item.get('value')[1:-1]
                if (type == 'NEWLINE' or type == 'NL'):
                   out.write(lineout + "\n")
                   lineout=" " * indent
                else:
                   if (type == 'INDENT'):
                       indent += 1
                       lineout=" " * indent
                   elif (type == 'DEDENT'):
                       indent -= 1
                       lineout=" " * indent
                   else:
                       lineout += value + " "
## it should be like this, but does not work                       
#                line = [type,value]
#                tokens.append(line)
##            [self.convert(element) for element in input]
#            print tokenize.untokenize( tokens)

            out.close()
            base = self.next_string(base) 

        
    def __init__(self,source):
        self.source = source    

    def buildMapper(self):
        for description in self.opClassDescription:
           for gene in description['genePool']:
               self.opMapper["'" + gene + "'" ] = description['name']

    def initAsTokenizer(self):
        self.tokens = []
        self.opMapper = {}
        self.opClassDescription = [
          OrderedDict([
            ('name','arithmetic'),
            ('type','symmetric'),
            ('genePool', ['/','-','*','+'])
          ]),
          OrderedDict([
            ('name','logical'),
            ('type','symmetric'),
            ('genePool', ['and','or'])
          ])

        ]
        self.classDescription =  [
            OrderedDict([
              ('name','NUMBER'),
              ('type', 'inmutable'),
              ('genes',[])
            ]),
            OrderedDict([
              ('name','ENDMARKER'),
              ('type', 'inmutable'),
              ('genes',[])
            ]),
            OrderedDict([
              ('name','NAME'),
              ('type', 'inmutable'),
              ('genes',[])
            ]),
            OrderedDict([
              ('name','OP'),
              ('type', 'inmutable'),
              ('genes',[])
            ]),
            OrderedDict([
              ('name','NEWLINE'),
              ('type', 'inmutable'),
              ('genes',[])
            ]),
            OrderedDict([
              ('name','INDENT'),
              ('type', 'inmutable'),
              ('genes',[])
            ]),
            OrderedDict([
              ('name','DEDENT'),
              ('type', 'inmutable'),
              ('genes',[])
            ]),
            OrderedDict([
              ('name','NL'),
              ('type', 'inmutable'),
              ('genes',[])
            ]),
            OrderedDict([
              ('name','COMMENT'),
              ('type', 'inmutable'),
              ('genes',[])
            ]),
            OrderedDict([
              ('name','STRING'),
              ('type', 'inmutable'),
              ('genes',[])
            ])

        ]
        self.buildMapper()         
    
    def handle_token(self, type, token, (srow,scol), (erow,ecol), line):
        #print "==%(type)s==%(token)s==\n" %  {'type': type, 'token':token}

        entry=OrderedDict([ 
           ('class',tokenize.tok_name[type]), 
           ('value',repr(token)), 
           ('info',[tokenize.tok_name[type],srow,scol,erow,ecol])
        ])
        
        self.tokens.append(entry)

    def tokenize(self):    
        tokenize.tokenize(self.source.readline,  self.handle_token)
        return self.tokens
        
    def classify(self):
      for index in range(len(self.tokens)):
        tokenType = filter (lambda x:x['name'] == self.tokens[index]['class'], self.classDescription)
        if tokenType == []:
          raise NameError(self.tokens[index]['class'])
        if tokenType[0]['name'] == 'OP' :
          key =  self.tokens[index]['value']
          if self.opMapper.has_key(key):
            self.tokens[index]['class']=self.opMapper[key]

    def tokensToJson(self):
        return json.dumps(self.tokens)
        
       
    def dump(self):
       self.tokenize()
       print len(self.tokens)
       self.classify()
       print len(self.tokens)
       return json.dumps(OrderedDict([("classes", self.classDescription + self.opClassDescription) ,( "tokens",self.tokens)]))

    # credit 956867 stackoverflow
    def convert(self, input):
        if isinstance(input,dict):
            return {self.convert(key):self.convert(value) for key, value in input.iteritems()}
        if isinstance(input,list):
            return [self.convert(element) for element in input]
        if isinstance(input, unicode):
            return input.encode('utf-8')
        return input

    # credit 932506 stackoverflow
    def next_string(self, s):
        strip_zs=s.rstrip('z')
        if strip_zs:
            return strip_zs[:-1] + chr(ord(strip_zs[-1]) + 1 ) + 'a' * (len(s) - len(strip_zs))
        return 'a' * (len(s) + 1)
