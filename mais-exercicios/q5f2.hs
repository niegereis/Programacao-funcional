higherOrderSum :: (Int -> Int) -> Int -> Int -> Int
higherOrderSum f limInf limSup =
  if limInf > limSup
    then 0
    else soma + higherOrderSum f (limInf + 1) limSup
  where
    soma = f limInf