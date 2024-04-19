multiplicaNoIntervalo m n 
  | m == n = n
  | otherwise = m * multiplicaNoIntervalo (m+1) n