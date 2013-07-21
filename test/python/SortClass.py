class SortClass:
  def __init__(self, list):
    self.list=list
    
  def mySwap(self,p1,p2):
    tmp = self.list[p1]
    self.list[p1]=self.list[p2]
    self.list[p2]=tmp


  def mySort(self):
    if (self.list[0]>self.list[1]):
      self.mySwap(self.list,0,1)
