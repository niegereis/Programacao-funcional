module Main(main) where
import System.IO(stdout, hSetBuffering, BufferMode(NoBuffering))

ehEleitor :: Integer -> String
ehEleitor idade 
  | idade < 16 = "não eleitor"
  | idade > 18 && idade < 65 = "eleitor obrigatório"
  | otherwise = "eleitor facultativo"

main :: IO()
main = do 
  hSetBuffering stdout NoBuffering
  putStrLn "Classe Eleitoral"
  putStrLn "-----------------------"
  putStrLn "Digite a idade da pessoa: "
  idadeEleitor <- readLn
  let verificacao = ehEleitor idadeEleitor
  putStrLn verificacao