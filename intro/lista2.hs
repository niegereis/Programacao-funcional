cteGravitaçãoUniversal :: Double
cteGravitaçãoUniversal = 6.67e-11

forçaGravidade :: Double -> Double -> Double -> Double
forçaGravidade m1 m2 d = cteGravitaçãoUniversal * m1 * m2 / d^2

forçaTerraLua :: Double
forçaTerraLua = forçaGravidade 6e24 1e23 4e5

salarioLiquido :: Double -> Double
salarioLiquido salarioBruto =  salarioBruto + (0.10 * salarioBruto) - (0.07 * salarioBruto)

ultimo lista = (lista) !! (length lista - 1)

primeiros lista = reverse (tail (reverse lista)) 

metade lista = (take ametade lista, drop ametade lista)
  where
    ametade = div (length lista) 2