fibonacci enesimoNum anterior antAnterior
  | enesimoNum == 1 = anterior 
  | otherwise = fibonacci (enesimoNum - 1) (anterior + antAnterior) anterior