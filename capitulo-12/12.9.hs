module Main where

import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

leNotas :: Int -> Int -> IO [(Float, Float, Float)]
leNotas n i
  | i > n = return []
  | otherwise = do
      putStrLn ("aluno " ++ show i)
      putStr " nota 1: "
      n1 <- readLn
      putStr " nota 2: "
      n2 <- readLn
      putStr " nota 3: "
      n3 <- readLn
      resto <- leNotas n (i + 1)
      return ((n1, n2, n3) : resto)

somaNotas :: [(Float, Float, Float)] -> Float
somaNotas ((nota1, nota2, nota3) : xs) =
  if null xs
    then soma
    else somaNotas xs + soma
  where
    soma = (nota1 + nota2 + nota3) / 3

mediaTurma :: [(Float, Float, Float)] -> Float
mediaTurma listaDeNotas = somaNotas listaDeNotas / fromIntegral (length listaDeNotas)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Fechamento de notas"
  putStrLn "====================================="
  putStr "Quantidade de alunos: "
  qtdeAlunos <- readLn
  notas <- leNotas qtdeAlunos 1
  let media = mediaTurma notas
  putStrLn ("Média da turma: " ++ show media)
