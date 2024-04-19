module Main (main) where
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

lerESomar qtdNum soma = do
  if qtdNum == 0 
    then return soma
    else do 
      putStrLn "Digite um número: "
      num <- readLn
      lerESomar (qtdNum - 1) (soma + num)
 

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Quantidade de números: "
  qtdNum <- readLn
  if qtdNum < 0 
    then putStrLn "Soma dos números: 0" 
    else do 
      soma <- lerESomar qtdNum 0
      putStrLn ("Soma dos números digitados " ++ show soma)