import GHC.IO.Handle (hSetBuffering)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

verificaTriangulo a b c = a > 0 && b > 0 && c > 0 && c > a + b && a > b + c && b > c + a

areaTriangulo a b c = (c * h) / 2
  where
    h = b * sinA
    sinA = sqrt (1 - (cosA ** 2))
    cosA = (b ** 2 + c ** 2 - a ** 2) / (2 * b * c)

main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Digite os valores dos lados a, b e c: "
  a <- readLn
  b <- readLn
  c <- readLn
  let ehTriangulo = verificaTriangulo a b c
  if ehTriangulo
    then do
      let area = areaTriangulo a b c
      putStrLn ("A area triangulo é: " ++ show area)
    else putStrLn "Valores inválidos! "