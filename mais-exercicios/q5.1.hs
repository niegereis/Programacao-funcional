data ArvBin t = Vazia | No t (ArvBin t) (ArvBin t)
               deriving (Show)

data MyMaybe a = MyNothing | MyJust a
               deriving (Show)


arvore1 = (No 5
              (No 9 Vazia Vazia)
              (No 5
                  (No 8
                      Vazia
                      (No 3 Vazia Vazia))
                  Vazia))

arvore2 = (No "paulo" Vazia (No "roberto" Vazia Vazia)) 

pesquisa :: Num a => (a -> Bool) -> ArvBin a -> MyMaybe a 
pesquisa _ Vazia = MyNothing
pesquisa func (No val esquerda direita) =
  if func val 
    then MyJust val 
    else case (pesquisa func esquerda, pesquisa func direita) of 
      (MyJust x, _) -> MyJust x
      (_, MyJust y) -> MyJust y
      _ -> MyNothing

pesquisa2 :: (Ord a) => (a -> Bool) -> ArvBin a -> MyMaybe a
pesquisa2 _ Vazia = MyNothing
pesquisa2 func (No val esquerda direita) = if length resultado >= 1 then MyJust (head resultado) else MyNothing
  where
    listaDosValores = arvLista (No val esquerda direita)
    resultado = filter func listaDosValores


-- somaArv :: Num a => ArvBin a -> a
somaArv Vazia = 0
somaArv (No val esquerda direita) = val + somaArv esquerda + somaArv direita

arvLista :: ArvBin a -> [a]
arvLista Vazia = []
arvLista (No val esquerda direita) = val : arvLista esquerda ++ arvLista direita


maxArvBin :: (Ord a) => ArvBin a -> MyMaybe a
maxArvBin Vazia = MyNothing
maxArvBin (No val esquerda direita) = MyJust maxValDaLista 
  where
    (x:xs) = arvLista (No val esquerda direita) 
    maxValDaLista = encontraMaiorValor xs x 

encontraMaiorValor :: (Ord a) => [a] -> a -> a
encontraMaiorValor [] val = val   
encontraMaiorValor xs val 
  | val > head xs = encontraMaiorValor (tail xs) val
  | val < head xs = encontraMaiorValor (tail xs) (head xs)
  | otherwise = encontraMaiorValor (tail xs) val  


elemento :: (Ord a) => a -> ArvBin a -> Bool
elemento _ Vazia = False
elemento queroEncontrar (No val esquerda direita) = (length valNalista) >= 1 
  where
    listaDeValores = arvLista (No val esquerda direita) 
    valNalista = filter (== queroEncontrar) listaDeValores
