module Main (main) where
import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))

main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Digite uma frase: "
  palavra <- getLine
  if palavra == reverse palavra
    then putStrLn "É uma palindrome."
    else putStrLn "Não é uma palíndrome"