data Figura
  = Retangulo Double Double
  | Circulo Double
  | Triangulo Double Double Double
  | Quadrado Double
  deriving (Show)

eRedondo :: Figura -> Bool
eRedondo (Circulo _) = True
eRedondo (Retangulo _ _) = False
eRedondo (Triangulo _ _ _) = False

perimetro :: Figura -> Double
perimetro (Retangulo b h) = (b * 2) + (h * 2)
perimetro (Circulo r) = r * pi * r
perimetro (Triangulo l1 l2 l3) = l1 + l2 + l3

area :: Figura -> Double
area (Retangulo b h) = b * h
area (Circulo r) = r * pi * r
area (Triangulo l1 l2 l3) = sqrt (p * (p - l1) * (p - l2) * (p - l3))
  where
    p = (l1 + l2 + l3) / 2

ehRegular :: Figura -> Bool
ehRegular (Quadrado _) = True
ehRegular (Circulo _) = True
ehRegular (Triangulo l1 l2 l3) = verifica
  where
    verifica = l1 == l2 && l2 == l3
