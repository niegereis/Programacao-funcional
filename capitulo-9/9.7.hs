module Main (main) where
import System.IO(hSetBuffering, stdout, BufferMode(NoBuffering))

op1 salario 
 | salario < 500 = salario * 0.05
 | salario < 850 = salario * 0.10
 | otherwise = salario * 0.15

op2 salario
 | salario > 1500 = salario + 25
 | salario >= 750 = salario + 50
 | salario >= 450 = salario + 75
 | otherwise = salario + 100 

op3 salario
 | salario <= 750 = "mal remunerado"
 | otherwise = "bem remunerado"

main = do 
  hSetBuffering stdout NoBuffering
  putStrLn "Opções: "
  putStrLn "1 - Imposto"
  putStrLn "2 - Novo salário"
  putStrLn "3 - Classificação"
  putStrLn "Digite a opção desejada: "
  op <- readLn
  if op == 1
    then do putStrLn "Digite o salário: "
            salario <- readLn
            let imposto = op1 salario
            putStrLn ("Imposto calculado: " ++ show imposto)
    else if op == 2
      then do putStrLn "Digite o salário: "
              salario <- readLn
              let novoSalario = op2 salario
              putStrLn ("Novo salário: " ++ show novoSalario)
      else if op == 3
          then do putStrLn "Digite o salário: "
                  salario <- readLn
                  let classificacao = op3 salario
                  putStrLn ("Classificação obtida: " ++ show classificacao)
        else putStrLn "Opçào inválida!"