module Main(main) where
import System.IO(hSetBuffering, stdout, BufferMode(NoBuffering))

ehTernoPitagorico n1 n2 n3
 | n1^2 + n2^2 == n3^2 = "Os números formam um terno pitagórico"
 | otherwise = "Os números não formam um terno pitagórico"

main = do 
  hSetBuffering stdout NoBuffering
  putStrLn "Verificação de ternos pitagóricos"
  putStrLn "-----------------------------------------"
  putStrLn "Digite o primeiro número positivo .....: "
  n1 <- readLn
  putStrLn "Digite o segundo número positivo .....: "
  n2 <- readLn
  putStrLn "Digite o terceiro número positivo .....: "
  n3 <- readLn
  if n1 >= 0 && n2 >= 0 && n3 >= 0
    then do let ehPitagorico = ehTernoPitagorico n1 n2 n3
            putStrLn ehPitagorico
    else putStrLn "Números inválidos"