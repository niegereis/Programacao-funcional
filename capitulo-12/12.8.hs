sucessor :: [Integer] -> Integer
sucessor lista
  | null lista = 0
  | otherwise = succ (head lista)

somaPriSec :: [Integer] -> Integer
somaPriSec listas
  | tamLista >= 2 = head listas + head (tail listas)
  | tamLista == 1 = head listas
  | otherwise = 0
  where
    tamLista = length listas