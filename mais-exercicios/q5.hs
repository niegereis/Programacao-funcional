somaNotas :: [Char] -> [Char] -> Integer
somaNotas [] [] = 0
somaNotas (g : gs) (gA : gAs) =
  if null gs
    then acertou
    else acertou + somaNotas gs gAs
  where
    acertou
      | g == gA = 1
      | otherwise = 0

calcNotas :: [Char] -> [(Integer, [Char])] -> [(Integer, Integer)]
calcNotas gabarito listaDeProvas =
  if null listaDeProvas
    then []
    else (fst (head listaDeProvas), notaAluno) : calcNotas gabarito (tail listaDeProvas)
  where
    gabaritoAluno = snd (head listaDeProvas)
    notaAluno = somaNotas gabarito gabaritoAluno
