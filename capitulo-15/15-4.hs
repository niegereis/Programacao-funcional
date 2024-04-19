composta :: (Num a, Integral b) => (a -> Bool) -> (b -> a)  -> (b -> Bool)
composta f g = fComposta
  where
    fComposta = (f . g) 