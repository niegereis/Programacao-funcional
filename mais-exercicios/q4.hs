produto m n =
  if m >= n
    then 1
    else m * produto (m + 1) n

produto' m n
  | m >= n = 1
  | otherwise = m * produto (m + 1) n