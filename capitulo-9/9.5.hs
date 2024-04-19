module Main(main) where
import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))

analisaCredito :: Double -> Double 
analisaCredito salario = salario * 0.3

main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Análise de crédito"
  putStrLn "------------------------------"
  putStrLn "Salário Bruto: "
  salario <- readLn
  putStrLn "Valor da prestação: "
  valPrestacao <- readLn
  let valPermitido = analisaCredito salario
  if valPrestacao <= valPermitido  
    then putStrLn "O empréstimo pode ser concedido"
    else putStrLn "O empréstimo nao pode ser concedido"
