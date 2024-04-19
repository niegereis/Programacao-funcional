module Main(main) where
import System.IO(hSetBuffering, stdout, BufferMode(NoBuffering))


perdaMassa massaInicial = massaInicial * (exp 1) ** (-5 * (10 ** (-2)) * 10) 

main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Digite a massa: "
  massa <- readLn
  let massaFinal = perdaMassa massa
  putStrLn ("Massa final: " ++ show massaFinal)
