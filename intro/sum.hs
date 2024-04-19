soma [] = 0
soma (x : xs) = x + soma xs

main :: IO ()
main = do
  print (soma [4, 10, 6])