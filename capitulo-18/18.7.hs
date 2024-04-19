data BinTree a
  = Vazia
  | No a (BinTree a) (BinTree a)
  deriving (Show)

btLength :: BinTree a -> Int
btLength Vazia = 0
btLength (No _ e d) = 1 + btLength e + btLength d

btDepth :: BinTree a -> Int
btDepth Vazia = 0
btDepth (No _ e d) = 1 + max (btDepth e) (btDepth d)

btElem :: (Eq a) => a -> BinTree a -> Bool
btElem _ Vazia = False
btElem x (No val e d) = if x == val then True else btElem x e || btElem x d
