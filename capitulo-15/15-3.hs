func :: (Num b) => Bool -> (b,b) -> b  
func = \a (m,n) -> if a then (m+n)^2 else (m+n)^3