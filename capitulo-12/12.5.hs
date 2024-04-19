opp :: (Int, (Int, Int)) -> Int
opp (x, (a, b))
  | (x, (a, b)) == (1, (a, b)) = a + b
  | (x, (a, b)) == (2, (a, b)) = a - b
  | otherwise = 0

opp @! (1, (a, b)) = a + b
opp @! (2, (a, b)) = a - b
opp @! _ = 0