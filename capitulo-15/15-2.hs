filtraMultiplos3 :: (Eq a, Integral a) => [a] -> [a]
filtraMultiplos3 = filter (\x -> if x `mod` 3 == 0 then True else False)  