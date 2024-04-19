listaDeNumeros :: [Integer] -> Integer
listaDeNumeros (x : y : _) = x + y
listaDeNumeros [x] = x
listaDeNumeros [] = 0