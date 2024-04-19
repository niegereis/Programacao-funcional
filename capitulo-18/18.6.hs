data Nat
  = Zero
  | Suc Nat
  deriving (Show)

zero :: Nat
zero = Zero

um :: Nat
um = Suc Zero

dois :: Nat
dois = Suc um

tres :: Nat
tres = Suc dois

nat2integer :: Nat -> Integer
nat2integer Zero = 0
nat2integer (Suc n) = 1 + nat2integer n

integer2nat :: Integer -> Nat
integer2nat 0 = Zero
integer2nat n = Suc (integer2nat (n - 1))

natLt :: Nat -> Nat -> Bool
natLt _ Zero = False
natLt Zero (Suc _) = True
natLt (Suc a) (Suc b) = natLt a b -- Nao entendi muito bem essa chamada recursiva
