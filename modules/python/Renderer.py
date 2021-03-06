import json

class Renderer:

    @classmethod    
    def newFromString(object,string,outputMask):
        rnd = Renderer(StringIO.StringIO(string),outputMask)
        return rnd

    @classmethod    
    def newFromFile(object,fileName, outputMask):
        rnd = Renderer(open(fileName,'r'),outputMask)
        return rnd
        
    def __init__(self,source,outputMask):
        self.source = source
        self.outputMask = outputMask
        
    def render(self):
        source = self.source
        json16 = json.load(source)
        json8 = self.convert(json16)
        base = '.aaaaaa'
        for file in json8:
            fileName = self. outputMask + base + ".py"
            print "####### " + base
            out = open(fileName, 'w')
            tokens=[]
            lineout=""
            indent = 0
            
            for item in file:
                type = item.get('info')[0]
                value = item.get('value')
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
                   #elif (type == 'OP'):
                       #lineout += value + " "
                   else:
                       lineout += value + " "
                       

                       
                       
## it should be like this, but does not work                       
#                line = [type,value]
#                tokens.append(line)
##            [self.convert(element) for element in input]
#            print tokenize.untokenize( tokens)

            out.close()
            base = self.next_string(base)
            
            
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