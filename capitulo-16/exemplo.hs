divisores :: (Integral a) => a -> [a]
divisores n = [x | x <- [1 .. n], mod n x == 0]

primo n = divisores n == [1, n]

primos n = [x | x <- [2 .. n], primo x]

primos' n = filter primo [2 .. n]