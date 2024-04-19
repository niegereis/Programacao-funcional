toDigits :: Integer -> [Integer]
toDigits numero =
  if numero == 0
    then []
    else toDigits numeroAtual ++ [mod numero 10]
  where
    numeroAtual = div numero 10

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther listaDeNumeros = map (\(v, i) -> if mod i 2 == 0 then v * 2 else v) (zip listaDeNumeros [0 ..])

sumDigits :: [Integer] -> Integer
sumDigits = foldl (\val acc -> if val > 10 then div val 10 + mod val 10 + acc else val + acc) 0

validate :: Integer -> Bool
validate numero = mod (sumDigits (doubleEveryOther (toDigits numero))) 10 == 0

-- 1234 = [4,3,2,1] = 10
-- 1234 = [4,3,2,1]
-- 1234 % 10 = 4 : [3,2,1]
-- 123 % 10 = 3 : [2,1]
-- 12 % 10 = 2 : [1]
-- 1 % 10 = 1 : []
-- 0 % 10 = []