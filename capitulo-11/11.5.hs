module Main(main) where
import System.IO(hSetBuffering, stdout, BufferMode(NoBuffering))

aumentoSalarial :: Double -> Integer -> Integer -> Double -> Double
aumentoSalarial salario anoCont anoAtual aumento =
  if anoCont == anoAtual
    then salario
    else aumentoSalarial (salario + salario * aumento) (anoCont + 1) anoAtual (aumento * 2)

main = do 
  hSetBuffering stdout NoBuffering
  putStrLn "Digite o ano de contratação: "
  foiContratado <- readLn
  putStrLn "Digite o ano de atual: "
  anoAt <- readLn
  putStrLn "Digite o salário: "
  salarioFunc <- readLn
  let newSalary = aumentoSalarial salarioFunc foiContratado anoAt (1.5/100)
  putStrLn ("Salário atual: " ++ show newSalary)
