module Main (main) where

import GHC.IO.Handle (BufferMode (NoBuffering), hSetBuffering)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "digite dois números: "
  x <- readLn
  y <- readLn
  let s = x + y
  putStrLn ("soma: " ++ show s)
