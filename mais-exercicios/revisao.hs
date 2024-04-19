-- Relembrando
dobro x = x + x
areaCirculo r = pi * r^2

-- Capitulo 3 - Exercícios
quadradoDoDobro x = (x + x) ** (2)

dobroDoQuadrado x = x ** 2 + x ** 2

ladosTriangulo a b c = a + b > c && b + c > a && c + a > b

areaTriangulo a b c = (c * h)/2
  where
    cosA = (b ** 2 + c ** 2 - a ** 2)/(2 * b * c)
    senA = sqrt (1 - cosA ** 2)
    h = b * sin a

-- Capitulo 4 - Exercícios
forcaGravidade m1 m2 dist = cteGravitaçãoUniversal * (m1 * m2/(dist ^ 2))
  where 
    cteGravitaçãoUniversal = 6.67 * (10 ** (-11) )

salario salFunc = salFunc + (salFunc * 0.10) - (salFunc * 0.07)

-- Capitulo 5 - Exercícios
func x y = if y == 0 then Nothing else Just (x/y)

ultimo lista = head (reverse lista)

primeiros lista = reverse (tail (reverse lista)) 

metade lista = (a , b)
  where
    metade = div (length lista)  2
    tamLista = length lista
    tamRestoLista = tamLista - metade
    a = take metade lista
    b = reverse (take tamRestoLista (reverse lista))

-- Capitulo 7 - Exercícios
areaDoCirculo :: Floating a => a -> a
areaDoCirculo r = pi * r ** 2

alturaDegraus:: (Integral b, RealFrac a) => a -> a -> b
alturaDegraus escada altura = ceiling (altura / escada)

verifica:: (Num a, Ord a) => a -> a -> a -> Bool
verifica a b c = b^2 - 4 * a *c >= 0

-- Capitulo 8 - Exercícios
max3:: (Num a, Ord a) => a -> a -> a -> a
max3 a b c = if a > b && a > c
              then a 
              else if b > c
                then b 
                else c

numRaizes:: (Num a, Ord a) => a -> a -> a -> a
numRaizes a b c 
  | delta > 0 = 2
  | delta == 0 = 1
  | otherwise = 0
  where
    delta = b^2 - 4 * a * c  

precoRetrato :: Integer -> String -> Double
precoRetrato numPessoas dia 
  | dia == "sabado" || dia == "domingo" = fimDeSemana
  | otherwise = semana 
  where
    fimDeSemana 
     | numPessoas == 1 = 100 + 0.20 * 100
     | numPessoas == 2 = 130 + 0.20 * 130
     | numPessoas == 3 = 150 + 0.20 * 150
     | numPessoas == 4 = 165 + 0.20 * 165
     | numPessoas == 5 = 175 + 0.20 * 175
     | numPessoas == 6 = 180 + 0.20 * 180
     | numPessoas >= 7 = 185 + 0.20 * 185
     | otherwise = 0
    semana
     | numPessoas == 1 = 100 
     | numPessoas == 2 = 130 
     | numPessoas == 3 = 150 
     | numPessoas == 4 = 165 
     | numPessoas == 5 = 175 
     | numPessoas == 6 = 180 
     | numPessoas >= 7 = 185 
     | otherwise = 0

conceito :: Float -> Float -> Float -> String
conceito lab nf ef 
  | nota >= 8 && nota <= 10 = "A"
  | nota >= 7 = "B"
  | nota >= 6 = "C"
  | nota >= 5 = "D"
  | nota >= 0 = "E"
  | otherwise = "I"
  where
    nota = (lab * 2 + nf * 3 + ef * 5)/10 


