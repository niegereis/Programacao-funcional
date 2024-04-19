module Main (main) where
import System.IO(hSetBuffering, stdout, BufferMode(NoBuffering))

criaListaDeAlunos :: [(Double, Double, Double)] -> Integer -> IO [(Double, Double, Double)]
criaListaDeAlunos lista qtdAlunos = do
  if qtdAlunos == 0
    then return lista
    else do
      putStrLn "Digite a nota 1, 2 e 3: "
      nota1 <- readLn 
      nota2 <- readLn
      nota3 <- readLn
      criaListaDeAlunos ((nota1, nota2, nota3) : lista) (qtdAlunos - 1)

notaFinalCadaAluno :: [(Double, Double, Double)] -> [Double] -> [Double]
notaFinalCadaAluno listaDeAlunos listaDeNota =
  if null listaDeAlunos 
    then listaDeNota
    else notaFinalCadaAluno (tail listaDeAlunos) ((nota1 + nota2 + nota3)/3 : listaDeNota) 
      where
        (nota1, nota2, nota3) = head listaDeAlunos

imprimeMediaAluno :: [Double] -> IO ()
imprimeMediaAluno listaDeNota = do
  if null listaDeNota
    then return ()
    else do
      let notaAluno = head listaDeNota
      putStrLn ("Media do aluno: " ++ show notaAluno)
      imprimeMediaAluno (tail listaDeNota)

obterSituacao :: Double -> String
obterSituacao media = situacao
  where
    situacao
      | media < 3 = "reprovado"
      | media >= 3 && media < 7 = "exame expecial"
      | otherwise = "aprovado"

situacaoAluno :: [Double] -> IO ()
situacaoAluno listaDeNota = do
  if null listaDeNota 
    then return ()
    else do 
      let media = head listaDeNota
      let situacao = obterSituacao media
      putStrLn ("Situação do aluno " ++ situacao)
      situacaoAluno (tail listaDeNota) 

percentualAlunosAprovados :: [Double] -> Integer -> Integer -> Double
percentualAlunosAprovados listaDeNota contAlunos qtdAlunos =
  if null listaDeNota
    then (fromIntegral contAlunos/(fromIntegral qtdAlunos) * 100)
    else
      if situacao == "aprovado" 
        then percentualAlunosAprovados (tail listaDeNota) (contAlunos + 1) qtdAlunos
        else percentualAlunosAprovados (tail listaDeNota) contAlunos qtdAlunos
        where
          media = head listaDeNota
          situacao = obterSituacao media

percentualAlunosExEXp :: [Double] -> Integer -> Integer -> Double
percentualAlunosExEXp listaDeNota contAlunos qtdAlunos =
  if null listaDeNota
    then (fromIntegral contAlunos/(fromIntegral qtdAlunos) * 100)
    else
      if situacao == "exame expecial" 
        then percentualAlunosExEXp (tail listaDeNota) (contAlunos + 1) qtdAlunos
        else percentualAlunosExEXp (tail listaDeNota) contAlunos qtdAlunos
      where
        media = head listaDeNota
        situacao = obterSituacao media

percentualAlunosReprovados :: [Double] -> Integer -> Integer -> Double
percentualAlunosReprovados listaDeNota contAlunos qtdAlunos =
  if null listaDeNota 
    then (fromIntegral contAlunos/(fromIntegral qtdAlunos) * 100)
    else
      if situacao == "reprovado" 
        then percentualAlunosReprovados (tail listaDeNota) (contAlunos + 1) qtdAlunos
        else percentualAlunosReprovados (tail listaDeNota) contAlunos qtdAlunos
      where
        media = head listaDeNota
        situacao = obterSituacao media


main = do 
  hSetBuffering stdout NoBuffering
  putStrLn "Digite a quantidade de alunos da turma: "
  qtdAlunos <- readLn
  listaDeAlunos <- criaListaDeAlunos [] qtdAlunos
  let listaDeNotas = notaFinalCadaAluno listaDeAlunos []
  imprimeMediaAluno listaDeNotas
  situacaoAluno listaDeNotas
  let alunosAprovados = percentualAlunosAprovados listaDeNotas 0 qtdAlunos
  putStrLn (show alunosAprovados ++ "% alunos aprovados.")
  let alunosExExp = percentualAlunosExEXp listaDeNotas 0 qtdAlunos
  putStrLn (show alunosExExp ++ "% alunos exame expecial.")
  let alunosReprovados = percentualAlunosReprovados listaDeNotas 0 qtdAlunos
  putStrLn (show alunosReprovados ++ "% alunos reprovados.")

