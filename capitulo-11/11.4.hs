module Main(main) where
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

valorPi :: Integer -> Double
valorPi n = 4 * (soma 0 n)

soma :: Integer -> Integer -> Double
soma limInf limSup =
  if limInf == limSup 
    then 0
    else ((-1) ** fromIntegral limInf)/ (2 * fromIntegral limInf + 1) + soma (limInf + 1) limSup

main = do 
  hSetBuffering stdout NoBuffering
  putStrLn "Digite o limSup: "
  nVal <- readLn
  let val = valorPi nVal
  putStrLn ("Valor de pi: " ++ show val)

