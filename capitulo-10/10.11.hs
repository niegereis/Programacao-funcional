potenciacao num exp
  | exp == 0 = 1
  | otherwise = num * potenciacao num (exp - 1)