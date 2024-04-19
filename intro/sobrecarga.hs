areaCir :: Floating a => a -> a
areaCir r = pi * (r^2)


-- possuiRaizes :: (Num a, Ord a) => a -> a -> a -> Bool
possuiRaizes a b c = b^2 - 4*a*c >= 0


max3 :: Integer -> Integer -> Integer -> Integer
max3 a b c = 
  if a > b && a > c 
    then a 
    else if b > c 
      then b 
      else c

precoRetrato :: Integer -> String -> Double 
precoRetrato qtdRetratos diaSemana = preco  + preco * taxaExtra
  where 
    preco | qtdRetratos == 1 = 100
          | qtdRetratos == 2 = 130
          | qtdRetratos == 3 = 150
          | qtdRetratos == 4 = 165
          | qtdRetratos == 5 = 175
          | qtdRetratos == 6 = 180
          | otherwise = 185

    taxaExtra | diaSemana == "Sábado" || diaSemana == "Domingo" = 0.2
              | otherwise = 0