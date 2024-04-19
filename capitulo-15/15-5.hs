-- composta :: (a -> a) -> (b -> a) -> b -> a
composta = (\f -> \g -> \x -> (f . g) x)