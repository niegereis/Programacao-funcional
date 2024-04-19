prefixo :: Int -> [a] -> [a]
prefixo num lista = case (num, lista) of
  (0, _) -> []
  (_, []) -> []
  (_, x : xs) -> x : prefixo (num - 1) xs
