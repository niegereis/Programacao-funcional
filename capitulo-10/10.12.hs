raizQuadrada num = raizQuadrada' num 1

raizQuadrada' num raiz 
 | num == raiz ^ 2 = raiz
 | num < raiz ^ 2 = (raiz - 1)
 | otherwise = raizQuadrada' num (raiz + 1) 