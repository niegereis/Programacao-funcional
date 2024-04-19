func1 :: Char
func1 = ((\a -> a) . head . head) ["maria","jose","silva"]

func2 :: Bool
func2 = (not . odd . length) "felicidade"

func3 :: Bool
func3 = ((\a -> True) . head . head . reverse) ["maria","silva","pereira"]

func4 :: Bool
func4 = (even . (\x -> x*2 + 3) . (\x -> div x 2) . snd) (9+4,9-4)