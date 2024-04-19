-- fatorialDuplo :: Integer -> Integer
fatorialDuplo n
  | n == 1  = 1
  | n == 2  = 2
  | otherwise = n * fatorialDuplo(n-2)