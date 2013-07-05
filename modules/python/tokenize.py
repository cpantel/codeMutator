import sys

def run():
  inputFile=sys.argv[1]
  outputFile=sys.argv[2]

  print inputFile

  f = open(outputFile, 'w')

  tok = Tokenizer.newFromFile(inputFile)
  got =  tok.dump()
#  f.write(tok.dump())
 
if __name__ == '__main__':
    from Tokenizer import Tokenizer
    run()


