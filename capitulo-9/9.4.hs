module Main (main) where
import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))

celsius :: Double -> Double
celsius t = 5/9 * (t - 32)

main = do 
  hSetBuffering stdout NoBuffering
  putStrLn "Temperatura em Fahrenheit: "
  temp <- readLn
  let tempC = celsius temp
  putStrLn ("Temperatura em Celsius: " ++ show tempC)  