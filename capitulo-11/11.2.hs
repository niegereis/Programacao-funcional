module Main(main) where 
import System.IO(hSetBuffering, stdout, BufferMode(NoBuffering))

leECalculamedia :: Double -> Double -> IO Double  
leECalculamedia soma cont = do 
  numLido <- readLn
  if numLido < 0
    then if cont > 0 
      then return (soma/cont)
      else return 0
    else do
      leECalculamedia (soma + numLido) (cont + 1) 

main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Cálculo da média aritmética"
  putStrLn "--------------------------------"
  putStrLn "Digite uma sequência de números (um por linha) "
  putStrLn "Para terminar digite um número negativo "
  media <- leECalculamedia 0 0
  if media == 0 
    then putStrLn "Sequência vazia"
    else putStrLn ("A média dos números digitados é " ++ show media)
