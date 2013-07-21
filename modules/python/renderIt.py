import sys

def run():
  inputFile=sys.argv[1]
  outputMask=sys.argv[2]

  print inputFile

  rnd = Renderer.newFromFile(inputFile, outputMask)
  rnd.render()  
 
if __name__ == '__main__':
    from Renderer import Renderer
    run()