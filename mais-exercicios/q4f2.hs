selEnquanto :: (a -> Bool) -> [a] -> [a]
selEnquanto f (x : xs) =
  if not retorno || null xs
    then if retorno then [x] else []
    else
      if retorno
        then x : selEnquanto f xs
        else selEnquanto f xs
  where
    retorno = f x