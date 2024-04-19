data Shop = Dados String Int Double

calcVal :: [Shop] -> Double
calcVal lista = foldl (+) 0 (map (\(Dados _ qtd preco) -> fromIntegral qtd * preco) lista)

-- calcVal [Dados "siuefhpvw" 2 2, Dados "ehgf9pwe" 1 2]